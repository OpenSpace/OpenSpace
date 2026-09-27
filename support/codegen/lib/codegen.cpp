/*****************************************************************************************
 *                                                                                       *
 * OpenSpace Codegen                                                                     *
 *                                                                                       *
 * Copyright (c) 2021-2026                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include "codegen.h"

#include "keywords.h"
#include "parsing.h"
#include "settings.h"
#include "snippets.h"
#include "types.h"
#include "verifier.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <iterator>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

using namespace std::literals;

// Set this value to true if you want to prevent changes to files during code refactoring
constexpr bool PreventFileChange = false;

template <>
struct std::formatter<std::filesystem::path> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const std::filesystem::path& path, std::format_context& ctx) const {
        return std::format_to(ctx.out(), "{}", path.string());
    }
};

namespace {
    std::vector<const VariableType*> usedTypes(const VariableType* var) {
        assert(var);
        std::vector<const VariableType*> res;
        res.push_back(var);

        switch (var->tag) {
            case VariableType::Tag::BasicType:
            case VariableType::Tag::PointerType:
            case VariableType::Tag::CustomType:
                break;
            case VariableType::Tag::MapType: {
                const MapType* mt = static_cast<const MapType*>(var);
                std::vector<const VariableType*> v1 = usedTypes(mt->keyType);
                std::vector<const VariableType*> v2 = usedTypes(mt->valueType);

                res.insert(res.end(), v1.begin(), v1.end());
                res.insert(res.end(), v2.begin(), v2.end());
                break;
            }
            case VariableType::Tag::OptionalType: {
                const OptionalType* pt = static_cast<const OptionalType*>(var);
                std::vector<const VariableType*> v1 = usedTypes(pt->type);
                res.insert(res.end(), v1.begin(), v1.end());
                break;
            }
            case VariableType::Tag::VariantType: {
                const VariantType* vt = static_cast<const VariantType*>(var);
                for (VariableType* v : vt->types) {
                    assert(v);
                    std::vector<const VariableType*> v1 = usedTypes(v);
                    res.insert(res.end(), v1.begin(), v1.end());
                }
                break;
            }
            case VariableType::Tag::TupleType: {
                const TupleType* vt = static_cast<const TupleType*>(var);
                for (VariableType* v : vt->types) {
                    assert(v);
                    std::vector<const VariableType*> v1 = usedTypes(v);
                    res.insert(res.end(), v1.begin(), v1.end());
                }
                break;
            }
            case VariableType::Tag::ArrayType: {
                const ArrayType* vt = static_cast<const ArrayType*>(var);
                std::vector<const VariableType*> v1 = usedTypes(vt->type);
                res.insert(res.end(), v1.begin(), v1.end());
                break;
            }
            case VariableType::Tag::VectorType: {
                const VectorType* vt = static_cast<const VectorType*>(var);
                std::vector<const VariableType*> v1 = usedTypes(vt->type);
                res.insert(res.end(), v1.begin(), v1.end());
                break;
            }
        }

        assert(
            std::none_of(
                res.begin(), res.end(),
                [](const VariableType* v) { return v == nullptr; }
            )
        );
        return res;
    }

    std::vector<const VariableType*> usedTypes(const Struct& s) {
        std::vector<const VariableType*> res;
        for (const Variable* var : s.variables) {
            assert(var);
            std::vector<const VariableType*> v = usedTypes(var->type);
            res.insert(res.end(), v.begin(), v.end());
        }

        for (StackElement* e : s.children) {
            assert(e);
            if (e->type != StackElement::Type::Struct) {
                continue;
            }

            const Struct& str = static_cast<const Struct&>(*e);
            std::vector<const VariableType*> t = usedTypes(str);
            res.insert(res.end(), t.begin(), t.end());
        }


        return res;
    }

    std::vector<const VariableType*> usedTypes(const std::vector<Struct*>& structs) {
        std::vector<const VariableType*> res;

        for (Struct* s : structs) {
            std::vector<const VariableType*> r = usedTypes(*s);
            res.insert(res.end(), r.begin(), r.end());
        }

        // Remove duplicates
        for (int i = 0; i < static_cast<int>(res.size()); i++) {
            for (int j = i + 1; j < static_cast<int>(res.size()); j++) {
                if (*res[i] == *res[j]) {
                    res.erase(res.begin() + j);
                    i--;
                    break;
                }
            }
        }

        return res;
    }

    std::vector<Enum*> mappedEnums(const Struct& s) {
        std::vector<Enum*> res;
        for (StackElement* se : s.children) {
            if (se->type == StackElement::Type::Enum) {
                Enum* e = static_cast<Enum*>(se);
                if (!e->attributes.mappedTo.empty()) {
                    res.push_back(e);
                }
            }

            if (se->type == StackElement::Type::Struct) {
                Struct* str = static_cast<Struct*>(se);
                std::vector<Enum*> subRes = mappedEnums(*str);
                res.insert(res.end(), subRes.begin(), subRes.end());
            }
        }

        return res;
    }

    std::string resolveComment(std::string comment) {
        if (auto [b, e] = findKeyword(comment, keywords::Verbatim);
            b != std::string::npos)
        {
            size_t it = e;
            if (comment[it] != '(') {
                throw CodegenError(std::format(
                    "Malformed codegen::verbatim. Expected ( after token\n{}", comment
                ));
            }
            it++;
            const size_t end = comment.find(')', it);
            const std::string argument = comment.substr(it, end - it);

            const size_t endKeyword = end + ")]]"sv.size();
            if (endKeyword - b == comment.size()) {
                // If there isn't any other comment, we are done and we emit the argument
                // to the verbatim function
                comment = argument;
            }
            else {
                // Otherwise, we need to output code that correctly inserts the verbatim
                // at the correct position
                const std::string pre = comment.substr(0, b);
                const std::string post = comment.substr(endKeyword);
                comment = std::format(
                    "std::format(\"{{}}{{}}{{}}\", \"{}\", {}, \"{}\")",
                    pre, argument, post
                );
            }
        }
        else {
            size_t jt = comment.find('"');
            while (jt != std::string::npos && comment[jt - 1] != '\\') {
                comment.insert(jt, "\\");
                jt = comment.find('"', jt + 2); // +1 for \\ and +1 for "
            }

            // We add artificial spaces between the multiline comments, which causes there
            // to be a stray space at the end
            if (!comment.empty() && comment.back() == ' ') {
                comment.pop_back();
            }
            comment = std::format("\"{}\"", comment);
        }
        return comment;
    }

    std::vector<Enum*> collectEnums(Struct* s) {
        std::vector<Enum*> res;
        for (StackElement* se : s->children) {
            switch (se->type) {
            case StackElement::Type::Struct:
            {
                std::vector<Enum*> e = collectEnums(static_cast<Struct*>(se));
                res.insert(res.end(), e.begin(), e.end());
                break;
            }
            case StackElement::Type::Enum:
                res.push_back(static_cast<Enum*>(se));
                break;
            }
        }
        return res;
    }

    std::vector<Enum*> collectEnums(const Code& code) {
        std::vector<Enum*> res = code.enums;

        for (Struct* s : code.structs) {
            std::vector<Enum*> e = collectEnums(s);
            res.insert(res.end(), e.begin(), e.end());
        }

        return res;
    }

    struct HeaderInfo {
        bool needsArrayifyFallback = false;
        bool needsMappingFallback = false;
        bool needsBakeEnumFallback = false;
        bool needsToStringFallback = false;
        bool needsFromStringFallback = false;

        bool needsAny() const {
            return needsArrayifyFallback || needsMappingFallback ||
                   needsBakeEnumFallback || needsToStringFallback ||
                   needsFromStringFallback;
        }
    };

    // Returns `true` if the variable has an attribute that the dedicated list verifier
    // cannot represent. This covers attributes that constrain the element (which the bare
    // list verifier would silently drop) as well as attributes that are invalid for the
    // element type (such as `reference` or `directory` on a string), which must still
    // flow through the normal path so that codegen reports the unsupported-attribute
    // error
    bool hasElementConstrainingAttribute(const Variable& var) {
        const Variable::Attributes& a = var.attributes;
        return !a.annotation.empty() || !a.inlist.empty() || !a.inrange.empty() ||
            !a.less.empty() || !a.lessequal.empty() || !a.greater.empty() ||
            !a.greaterequal.empty() || !a.notinlist.empty() || !a.notinrange.empty() ||
            !a.reference.empty() || !a.unequal.empty() || a.isColor || a.isDateTime ||
            a.isIdentifier || a.mustBeNotEmpty || a.isDirectory;
    }

    std::string verifier(VariableType* type, const Variable& var, Struct* currentStruct) {
        assert(type);
        assert(currentStruct);

        if (type->tag == VariableType::Tag::BasicType) {
            BasicType* bt = static_cast<BasicType*>(type);
            const std::string v = verifierForType(bt->type, var.attributes);
            return "new " + v;
        }
        else if (type->tag == VariableType::Tag::OptionalType) {
            OptionalType* ot = static_cast<OptionalType*>(type);
            return verifier(ot->type, var, currentStruct);
        }
        else if (type->tag == VariableType::Tag::VectorType) {
            VectorType* vt = static_cast<VectorType*>(type);
            const StackElement* e = resolveType(
                currentStruct,
                generateTypename(vt->type)
            );
            std::string comments;
            if (e) {
                // `e` is false for subtypes that are not our own structs
                comments = resolveComment(e->comment);
            }

            // Use a dedicated list verifier for a vector of strings, but only when there
            // are no attributes constraining the individual elements. The list verifier
            // cannot express such constraints, so in that case we fall through to the
            // generic TableVerifier path below, which applies the attributes to each
            // element
            const bool isBasic = vt->type->tag == VariableType::Tag::BasicType;
            if (isBasic && !hasElementConstrainingAttribute(var)) {
                BasicType* bt = static_cast<BasicType*>(vt->type);
                if (bt->type == BasicType::Type::String) {
                    return std::format("new StringListVerifier({})", comments);
                }
            }

            std::string ver = verifier(vt->type, var, currentStruct);
            return std::format(
                "new TableVerifier({{{{\"*\",{},Optional::Yes,Private::No,{}}}}})",
                ver, comments
            );
        }
        else if (type->tag == VariableType::Tag::ArrayType) {
            ArrayType* at = static_cast<ArrayType*>(type);
            const StackElement* e = resolveType(
                currentStruct,
                generateTypename(at->type)
            );
            std::string comments;
            if (e) {
                // `e` is false for subtypes that are not our own structs
                comments = resolveComment(e->comment);
            }

            std::string ver = verifier(at->type, var, currentStruct);
            return std::format(
                "new TableVerifier({{{{\"*\",{},Optional::No,Private::No,{}}}}}, {})",
                ver, comments, at->size
            );
        }
        else if (type->tag == VariableType::Tag::VariantType) {
            VariantType* vt = static_cast<VariantType*>(type);

            std::string result = "new OrVerifier({";

            for (VariableType* v : vt->types) {
                result += verifier(v, var, currentStruct) + ',';
            }

            // Remove the final ,
            result.pop_back();

            result += "})";
            return result;
        }
        else if (type->tag == VariableType::Tag::MapType) {
            MapType* mt = static_cast<MapType*>(type);
            if (mt->valueType->tag == VariableType::Tag::BasicType) {
                BasicType* valueType = static_cast<BasicType*>(mt->valueType);
                std::string verifier = verifierForType(valueType->type, var.attributes);
                return std::format(
                    "new TableVerifier({{{{\"*\",new {}}}}})", verifier
                );
            }
            else {
                std::string valueVerifier = verifier(mt->valueType, var, currentStruct);
                // No `new` in this format string as the `verifier` function above will
                // already have added that part
                return std::format(
                    "new TableVerifier({{{{\"*\", {}}}}})", valueVerifier
                );
            }
        }
        else if (type->tag == VariableType::Tag::TupleType) {
            TupleType* tt = static_cast<TupleType*>(type);
            std::string res = "new TableVerifier({";
            for (size_t i = 0; i < tt->types.size(); i++) {
                VariableType* v = tt->types[i];
                std::string ver = verifier(v, var, currentStruct);
                // +1 to accommodate 1-based counting for Lua
                res += std::format("{{\"{}\",{} }},", i + 1, ver);
            }

            res += std::format("}}, {})", tt->types.size());
            return res;
        }
        else if (type->tag == VariableType::Tag::CustomType) {
            CustomType* ct = static_cast<CustomType*>(type);
            assert(ct->type);
            return std::format("codegen_{}_{}", fqn(currentStruct, "_"), ct->type->name);
        }
        else {
            throw std::logic_error("Missing case label");
        }
    }

    std::string writeEnumDocumentation(Enum* e) {
        std::string result = std::format(
            "    std::shared_ptr<StringInListVerifier> codegen_{} = std::make_shared<StringInListVerifier>(std::vector<std::string>{{",
            fqn(e, "_")
        );

        for (EnumElement* em : e->elements) {
            // If no key attribute is specified, we use the name instead
            std::string key =
                em->attributes.key.empty() ?
                std::format("\"{}\"", em->name) :
                em->attributes.key;
            result += std::format("{},", key);
        }
        // The last element has a , at the end that we can overwrite
        result.pop_back();
        result += "});\n";

        return result;
    }

    std::string writeVariableDocumentation(Struct* s, Variable* var) {
        const bool isOptional = var->type->tag == VariableType::Tag::OptionalType;

        std::string ver = fqn(s, "_");
        std::string v = verifier(var->type, *var, s);
        if (var->comment.empty()) {
            std::string result = std::format(
                "    codegen_{}->documentations.emplace_back({},{},{},{});\n",
                ver,
                var->key,
                v,
                isOptional ? "Optional::Yes" : "Optional::No",
                var->attributes.isPrivate ? "Private::Yes" : "Private::No"
            );
            return result;
        }
        else {
            var->comment = resolveComment(var->comment);
            std::string result = std::format(
                "    codegen_{}->documentations.emplace_back({},{},{},{},{});\n",
                ver,
                var->key,
                v,
                isOptional ? "Optional::Yes" : "Optional::No",
                var->attributes.isPrivate ? "Private::Yes" : "Private::No",
                var->comment
            );
            return result;
        }
    }

    std::string writeStructDocumentation(Struct* s) {
        std::string name = fqn(s, "_");
        std::string result;
        if (s->parent) {
            result = std::format(
                "    std::shared_ptr<TableVerifier> codegen_{} = std::make_shared<TableVerifier>();\n", name
            );
        }
        else {
            // Root struct
            result = std::format(
                "    TableVerifier codegen_{0}_content;\n"
                "    TableVerifier* codegen_{0} = &codegen_{0}_content;\n"
                , name
            );
        }

        for (StackElement* e : s->children) {
            if (e->type == StackElement::Type::Struct) {
                result += writeStructDocumentation(static_cast<Struct*>(e));
            }
            if (e->type == StackElement::Type::Enum) {
                result += writeEnumDocumentation(static_cast<Enum*>(e));
            }
        }

        for (Variable* var : s->variables) {
            result += writeVariableDocumentation(s, var);
        }

        return result;
    }

    std::string writeVariantConverter(Variable* var, std::vector<std::string>& converters)
    {
        VariableType* type = var->type;
        if (var->type->tag == VariableType::Tag::OptionalType) {
            // For this case, we don't care about whether the variant is wrapped in an
            // optional. The converter code generated for the optional<T> will call the
            // code generated here. So we basically just unwrap the optional type here
            OptionalType* ot = static_cast<OptionalType*>(var->type);
            type = ot->type;
        }

        if (type->tag != VariableType::Tag::VariantType) {
            // No need to even look at non-variant types here
            return "";
        }
        VariantType* variantType = static_cast<VariantType*>(type);

        std::string typeString = generateTypename(type, true);
        if (auto it = std::find(converters.begin(), converters.end(), typeString);
            it != converters.end())
        {
            // This check will trigger if we are using the same type of variant for
            // multiple variables in the same struct. If that is the case, we only want to
            // emit the conversion code once, or else we would get a multiply defined
            // function definition compile error
            return "";
        }
        converters.push_back(typeString);

        std::string res = std::format(
            "[[maybe_unused]] "
            "void bakeTo(const ghoul::Dictionary& d, std::string_view key, {}* val) {{\n",
            typeString
        );

        for (VariableType* t : variantType->types) {
            const std::string typeName = generateTypename(t);

            const bool isEnumType =
                t->isCustomType() &&
                static_cast<CustomType*>(t)->type->type == StackElement::Type::Enum;
            if (t->isVectorType()) {
                const std::string bakeFunction = vectorBakeFunctionForType(typeName);
                res += bakeFunction;
            }
            else if (isEnumType) {
                const std::string fqnType = fqn(static_cast<CustomType*>(t)->type, "::");
                const std::string bakeFunction = enumBakeFunctionForType(fqnType);
                res += bakeFunction;
            }
            else {
                const std::string_view convFunc =
                    variantConversionFunctionForType(typeName);
                assert(!convFunc.empty());
                res += convFunc;
            }
        }

        res += "    // Any of the previous values should have triggered and returned\n";
        res += "    // If this assert triggers, something in the codegen went wrong\n";
        res += "    assert(false);\n";
        res += "}\n";
        return res;
    }

    std::string writeInnerEnumConverter(Enum* e) {
        std::string type = fqn(e, "::");
        std::string result = std::format(
            "[[maybe_unused]] "
            "void bakeTo(const ghoul::Dictionary& d, std::string_view key, {}* val) {{\n"
            "    std::string v = d.value<std::string>(key);\n",
            type
        );

        for (EnumElement* elem : e->elements) {
            assert(elem);
            std::string typeStr = fqn(e, "::");
            assert(!elem->attributes.key.empty());
            result += std::format(
                "    if (v == {}) {{ *val = {}::{}; }}\n",
                elem->attributes.key, typeStr, elem->name
            );
        }
        result += "}\n";

        return result;
    }

    std::string writeEnumConverterToString(const Enum* e) {
        assert(e);
        std::string res = std::format(R"(
    template <> [[maybe_unused]] std::string_view toString<{0}>({0} t) {{
        switch (t) {{
    )",
    fqn(e, "::")
    );
        for (EnumElement* elem : e->elements) {
            res += std::format(
                "        case {0}::{1}: return {2};\n",
                fqn(e, "::"), elem->name, elem->attributes.key
            );
        }
        res += R"(        default: throw "This default label is not necessary since the case labels are "
                           "exhaustive, but not having it makes Visual Studio cranky";)";
        res += "\n    }\n}\n";
        return res;
    }

    std::string writeEnumConverterFromString(const Enum* e) {
        assert(e);

        std::string res = std::format(
            "template <> [[maybe_unused]] {0} fromString<{0}>(std::string_view sv) {{\n",
            fqn(e, "::")
        );
        for (EnumElement* elem : e->elements) {
            res += std::format("    if (sv == {0}) {{ return {1}::{2}; }}\n",
                elem->attributes.key, fqn(e, "::"), elem->name
            );
        }
        res += "    throw std::runtime_error(std::format(\"Could not find value '{}' in enum\", sv));\n";
        res += "}\n";
        return res;
    }

    std::string writeEnumConverterBake(const Enum* e) {
        return std::format(BakeEnum, fqn(e, "::"));
    }

    std::string writeStructConverter(Struct* s,
                                     std::vector<std::string>& writtenVariantConverters)
    {
        assert(s);
        std::string result;

        for (StackElement* el : s->children) {
            assert(el);
            if (el->type == StackElement::Type::Struct) {
                Struct* sl = static_cast<Struct*>(el);
                result += writeStructConverter(sl, writtenVariantConverters);
            }

            if (el->type == StackElement::Type::Enum) {
                Enum* e = static_cast<Enum*>(el);
                result += writeInnerEnumConverter(e);
            }
        }

        for (Variable* var : s->variables) {
            assert(var);
            result += writeVariantConverter(var, writtenVariantConverters);
        }

        if (s->parent == nullptr) {
            return result;
        }

        std::string name = fqn(s, "::");
        result += std::format(R"(template <> [[maybe_unused]] void bakeTo<{0}>(const ghoul::Dictionary& d, std::string_view key, {0}* val) {{
        {0}& res = *val;
        ghoul::Dictionary dict = d.value<ghoul::Dictionary>(key);
    )",
            name
        );

        for (Variable* var : s->variables) {
            result += std::format(
                "    internal::bakeTo(dict, {}, &res.{});\n", var->key, var->name
            );
        }

        result += "}\n";
        return result;
    }

    std::string emitWarningsForDocumentationLessTypes(Struct* s,
                                                      std::string_view sourceFile)
    {
        assert(s);

        std::string res;
        for (Variable* var : s->variables) {
            assert(var);
            if (var->comment.empty()) {
                std::string identifier = std::format("{}.{}", fqn(s, "."), var->name);
                std::string message = std::format(
                    "\"{}: [CODEGEN] {} is not documented\"", sourceFile, identifier
                );
#ifdef WIN32
                res += std::format("#pragma message({})\n", message);
#else // ^^^^ WIN32 // !WIN32 vvvv
                res += std::format("#warning {}\n", message);
#endif // WIN32
            }
        }
        return res;
    }


    std::string generateStructsResult(const Code& code, HeaderInfo& info) {
        // For Linux, we need to declare the functions in the following order or the
        // overload resolution picks the top fall back implentation and triggers a
        // static_assert:
        // 1. <typename T> bakeTo(..., T*) { static_assert(false); } // fallback
        // 2. <typename T> bakeTo(..., std::optional<T>*)   declaration only
        // 3. <typename T> bakeTo(..., std::vector<T>*)   declaration only
        // 4. bakeTo(..., T*) {...} for T=base types
        // 5. <typename T> bakeTo(..., T*) {...}  for T=custom structs
        // 6. <typename T> bakeTo(..., std::optional<T>*) {...}   definition
        // 7. <typename T> bakeTo(..., std::vector<T>*) {...}   definition
        //
        // Reason:  When going through the custom structs, they need to know that there
        // will be implementatinos for the optional and vector types or otherwise they
        // will trip the fall back.  However, the actual implementation needs to know
        // about the custom struct one or else *they* will trip the fall back in the
        // implementation. For ease of implementation, we are putting 3&4 before 2 instead

        if (code.structs.empty()) {
            return "";
        }

        std::string result;
        result += "namespace codegen {\n\n";
        result += DocumentationFallback;
        result += DocumentationPackOverload;

        for (Struct* s : code.structs) {
            result += std::format(DocumentationPreamble, s->name);

            if (GenerateWarningsForDocumentationLessTypes) {
                result += emitWarningsForDocumentationLessTypes(s, code.sourceFile);
            }

            result += writeStructDocumentation(s);
            result += std::format(
                DocumentationEpilog,
                s->attributes.dictionary, s->name, s->comment
            );
        }

        result += "namespace internal {\n\n";
        result += BakeToFunctionFallback;
        result += "\n\n";

        const std::vector<const VariableType*> types = usedTypes(code.structs);
        bool hasOptionalType = false;
        bool hasVectorType = false;
        bool hasArrayType = false;
        bool hasMapStringKeyType = false;
        bool hasMapEnumKeyType = false;
        bool hasTupleType = false;
        for (const VariableType* t : types) {
            assert(t);
            hasOptionalType |= t->isOptionalType();
            hasVectorType |= t->isVectorType();
            hasArrayType |= t->isArrayType();
            hasTupleType |= t->isTupleType();

            if (t->isMapType()) {
                const MapType* mt = static_cast<const MapType*>(t);
                hasMapStringKeyType |= mt->hasStringKey();
                hasMapEnumKeyType |= mt->hasEnumKey();
            }
        }

        if (hasOptionalType) {
            result += BakeFunctionOptionalDeclaration;
        }
        if (hasVectorType) {
            result += BakeFunctionVectorDeclaration;
        }
        if (hasArrayType) {
            result += BakeFunctionArrayDeclaration;
        }
        if (hasMapStringKeyType) {
            result += BakeFunctionMapStringKeyDeclaration;
        }
        if (hasMapEnumKeyType) {
            info.needsFromStringFallback = true;
            result += BakeFunctionMapEnumKeyDeclaration;
        }
        if (hasTupleType) {
            result += BakeFunctionTupleDeclaration;
        }
        for (const VariableType* t : types) {
            if (t->isBasicType()) {
                const BasicType* bt = static_cast<const BasicType*>(t);
                result += bakeFunctionForType(bt->type);
            }
        }

        std::vector<std::string> writtenVariantConverters;
        for (Struct* s : code.structs) {
            result += writeStructConverter(s, writtenVariantConverters);
        }

        if (hasOptionalType) {
            result += BakeFunctionOptional;
        }
        if (hasVectorType) {
            result += BakeFunctionVector;
        }
        if (hasArrayType) {
            result += BakeFunctionArray;
        }
        if (hasMapStringKeyType) {
            result += BakeFunctionMapStringKey;
        }
        if (hasMapEnumKeyType) {
            result += BakeFunctionMapEnumKey;
        }
        if (hasTupleType) {
            result += BakeFunctionTuple;
        }

        result += "\n} // namespace internal\n\n";
        result += BackFunctionFallback;
        result += '\n';

        for (Struct* s : code.structs) {
            result += std::format(BakeStructPreamble, s->name, s->attributes.dictionary);

            for (Variable* var : s->variables) {
                result += std::format(
                    "    internal::bakeTo(dict, {}, &res.{});\n", var->key, var->name
                );
            }

            result += "    return res;\n}\n";

            const std::vector<Enum*> enums = mappedEnums(*s);
            if (!enums.empty()) {
                info.needsMappingFallback = true;
            }

            for (Enum* e : enums) {
                result += enumToEnumMapping(e);
            }
        }

        result += "\n} // namespace codegen\n\n";
        return result;
    }

    std::string generateEnumResult(const Code& code, HeaderInfo& info) {
        const std::vector<Enum*> enums = collectEnums(code);

        std::string result;

        // First writing out the the enums defined at the root
        for (Enum* e : code.enums) {
            result += writeEnumConverterToString(e);
            info.needsToStringFallback = true;
            result += writeEnumConverterFromString(e);
            info.needsFromStringFallback = true;
            result += writeEnumConverterBake(e);
            info.needsBakeEnumFallback = true;

            if (!e->attributes.mappedTo.empty()) {
                info.needsMappingFallback = true;
                result += enumToEnumMapping(e);
            }

            if (e->attributes.arrayify) {
                info.needsArrayifyFallback = true;
                result += enumArrayify(e);
            }
        }

        // Then there might be enums that are defined inside structs that are used as keys
        // in maps. If that is the case, we also need to write out the fromString function
        // for these; but _only_ if they are not also included in the list above so that
        // we don't write out the fromString method twice
        std::vector<const Enum*> writtenEnums;
        for (Struct* s : code.structs) {
            std::vector<const VariableType*> variables = usedTypes(*s);
            for (const VariableType* vt : variables) {
                if (!vt->isMapType()) {
                    continue;
                }

                const MapType* mt = static_cast<const MapType*>(vt);
                if (!mt->hasEnumKey()) {
                    continue;
                }

                assert(mt->keyType->isCustomType());
                CustomType* ct = static_cast<CustomType*>(mt->keyType);
                assert(ct->type->type == StackElement::Type::Enum);
                const Enum* e = static_cast<const Enum*>(ct->type);

                auto it = std::find(code.enums.begin(), code.enums.end(), e);
                auto it2 = std::find(writtenEnums.begin(), writtenEnums.end(), e);
                if (it == code.enums.end() && it2 == writtenEnums.end()) {
                    result += writeEnumConverterFromString(e);
                    info.needsFromStringFallback = true;
                    writtenEnums.push_back(e);
                }
            }
        }

        if (result.empty()) {
            return "";
        }
        else {
            return std::format(
                "namespace codegen {{\n{}\n }} // namespace codegen\n\n",
                result
            );
        }
    }

    std::string generateLuaFunction(Function* f) {
        assert(f);
        std::string result;
        // Open the Function object declaration
        std::string capitalizedName = f->functionName;
        capitalizedName[0] = static_cast<char>(::toupper(capitalizedName[0]));

        result += std::format(LuaWrapperPreamble, capitalizedName);
        result += std::format("    \"{}\",\n", f->luaName);

        // The lambda that is executed
        result += "    [](lua_State* L) -> int {\n";
        result += std::format("        ZoneScopedN(\"[Lua] {}\");\n", f->functionName);

        int nRequiredArguments = 0;
        for (Variable* var : f->arguments) {
            if (!var->type->isOptionalType()) {
                nRequiredArguments += 1;
            }
        }

        // Adding the check for number of variables
        int nTotalArguments = static_cast<int>(f->arguments.size());
        if (nRequiredArguments == nTotalArguments) {
            result += std::format(
                "        ghoul::lua::checkArgumentsAndThrow(L, {}, \"{}\");\n",
                nTotalArguments, f->luaName
            );
        }
        else {
            result += std::format(
                "        ghoul::lua::checkArgumentsAndThrow(L, {{ {}, {} }}, \"{}\");\n",
                nRequiredArguments, nTotalArguments, f->luaName
            );
        }

        std::vector<Variable*> arguments = f->arguments;
        // If we only have optional arguments, we don't want them to be peeled off from
        // the front, but handle them as a bunch instead
        bool hasOnlyOptional = true;
        for (Variable* var : arguments) {
            if (!var->type->isOptionalType()) {
                hasOnlyOptional = false;
                break;
            }
        }

        // Need to handle optional arguments separately from the regular ones as they have
        // to have an extra check to see if the value exists
        while (!hasOnlyOptional &&
               !arguments.empty() && arguments.front()->type->isOptionalType())
        {
            Variable* var = arguments.front();
            OptionalType* ot = static_cast<OptionalType*>(var->type);

            result += std::format(
                LuaWrapperOptionalTypeExtraction,
                generateTypename(ot), var->name, generateLuaExtractionTypename(ot->type)
            );

            // Peel off the argument that we just handled
            arguments.erase(arguments.begin());
        }

        if (!arguments.empty()) {
            std::string names;
            std::string types;
            for (Variable* var : arguments) {
                names += var->name + ", ";
                types += generateLuaExtractionTypename(var->type) + ", ";
            }
            names = names.substr(0, names.size() - 2);
            types = types.substr(0, types.size() - 2);

            result += std::format(
                "        auto [{}] = ghoul::lua::values<{}>(L);\n", names, types
            );
        }

        result += "        try {\n";

        // Depending on if there is a return value, we want to be able to capture it
        result += "            ";
        if (f->returnValue) {
            result += std::format("{} res = ", generateTypename(f->returnValue));
        }

        if (f->arguments.empty()) {
            // If there are no arguments to the function, it's pretty simple to just call
            // it
            result += std::format("{}();\n", f->functionName);
        }
        else {
            // If there are arguments it might get a bit more complicated since we want to
            // support default initialized arguments
            result += std::format("{}(\n", f->functionName);

            for (size_t i = 0; i < f->arguments.size(); i += 1) {
                Variable* var = f->arguments[i];

                result += "                ";
                if (var->type->isOptionalType() &&
                    static_cast<OptionalType*>(var->type)->defaultArgument.has_value())
                {
                    OptionalType* ot = static_cast<OptionalType*>(var->type);
                    // If there is a default argument, the function actually wants the
                    // underlying value, not the std::optional type. We basically check if
                    // the original optional value was set; if it was, we dereference it
                    // and use it.
                    // Otherwise we paste in the stored default value instead.
                    //
                    // An alternative way would be to not provide the optional argument at
                    // all, but that turned out to be much more code and more complicated
                    // than just storing the default value
                    result += std::format(
                        "{0}.has_value() ? std::move(*{0}) : {1}",
                        var->name, *ot->defaultArgument
                    );
                }
                else if (var->type->containsCustomType()) {
                    // We have extracted this type as a ghoul::Dictionary previously, and
                    // need to bake it into the correct type here instead
                    result += std::format(
                        "codegen::bake<{0}>({1})",
                        generateTypename(var->type), var->name
                    );
                }
                else {
                    result += std::format("std::move({})", var->name);
                }

                if (i != f->arguments.size() - 1) {
                    result += ',';
                }
                result += '\n';
            }
            result += "            );\n";
        }

        if (f->returnValue) {
            if (f->returnValue->isOptionalType()) {
                result += LuaWrapperPushOptional;
            }
            else if (f->returnValue->isVariantType()) {
                VariantType* vt = static_cast<VariantType*>(f->returnValue);
                for (VariableType* v : vt->types) {
                    result += std::format(LuaWrapperPushVariant, generateTypename(v));
                }
                result += "            return 1;\n";
            }
            else if (f->returnValue->isCustomType()) {
                CustomType* vt = static_cast<CustomType*>(f->returnValue);

                switch (vt->type->type) {
                    case StackElement::Type::Struct: {
                        const Struct* s = static_cast<const Struct*>(vt->type);

                        result += "            lua_newtable(L);\n";
                        for (Variable* var : s->variables) {
                            result += std::format(
                                "            ghoul::lua::push(L, \"{0}\", std::move(res.{0}));\n",
                                var->name
                            );
                            result += "            lua_settable(L, -3);\n";
                        }
                        result += '\n';
                        result += "            return 1;\n";
                        break;
                    }
                    case StackElement::Type::Enum: {
                        result += "            ghoul::lua::push(L, codegen::toString(res));\n";
                        result += "            return 1;\n";
                        break;
                    }
                }
            }
            else {
                result += "            ghoul::lua::push(L, std::move(res));\n";
                result += "            return 1;\n";
            }
        }
        else {
            result += "            return 0;\n";
        }

        result += "        }\n";
        result += "        catch (const ghoul::lua::LuaError& error) {\n";
        result += "            return ghoul::lua::luaError(L, error.message);\n";
        result += "        }\n";

        result += "    },\n";


        // Argument description
        std::string argumentsDesc = "{\n";
        for (Variable* var : f->arguments) {
            argumentsDesc += std::format(
                R"(        {{ "{}", "{}")",
                var->name, generateDescriptiveTypename(var->type)
            );

            if (var->type->isOptionalType() &&
                static_cast<OptionalType*>(var->type)->defaultArgument.has_value())
            {
                OptionalType* ot = static_cast<OptionalType*>(var->type);
                std::string defaultArgument = *ot->defaultArgument;
                for (size_t i = 0; i < defaultArgument.size(); i += 1) {
                    if (defaultArgument[i] == '\\' || defaultArgument[i] == '"') {
                        defaultArgument.insert(defaultArgument.begin() + i, '\\');
                        i += 1;
                    }
                    if (defaultArgument[i] == '\n') {
                        defaultArgument.erase(defaultArgument.begin() + i);
                        i -= 1;
                    }
                }
                argumentsDesc += std::format(", \"{}\"", defaultArgument);
            }

            argumentsDesc += " },\n";

        }
        if (!f->arguments.empty()) {
            // Remove the closing ", "
            argumentsDesc = argumentsDesc.substr(0, argumentsDesc.size() - 1);
        }

        result += "    " + argumentsDesc + "\n    },\n";

        result += std::format(
            "    \"{}\",\n",
            f->returnValue ? generateDescriptiveTypename(f->returnValue) : ""
        );

        // Documentation
        result += std::format(R"(    R"[({})[")", f->documentation) + ",\n";

        // Source location
        if (f->sourceLocation.file.empty()) {
            // The `sourceLocation.file` is empty for all of the unit tests where we call
            // the `parse` function manually, so that not filename exists
            result += std::format("    {{ \"<null>\", {} }}\n", f->sourceLocation.line);
        }
        else {
            // `sourceLocation.file` is the full path to the file, but we only want to
            // store the path relative to the working directory
            result += std::format(
                "    {{ \"{}\", {} }}\n",
                f->sourceLocation.file.string(), f->sourceLocation.line
            );
        }

        result += "};\n\n";

        return result;
    }

    std::string generateLuaWrapperResult(const Code& code, HeaderInfo&) {
        if (code.luaWrapperFunctions.empty()) {
            return "";
        }

        std::string result;

        // The Lua wrapping functions require a bit more of the conversions, like being
        // able to return them wrapped in an optional, vector, etc, so we need to enhance
        // the `bake` function to be able to take care of those types if they arise
        bool hasCustomInMap = false;
        bool hasCustomInOptional = false;
        bool hasCustomInVector = false;

        auto updateState = [&](VariableType* var) {
            if (!var) {
                return;
            }
            hasCustomInMap |= var->isMapType() && var->containsCustomType();
            hasCustomInOptional |= var->isOptionalType() && var->containsCustomType();
            hasCustomInVector |= var->isVectorType() && var->containsCustomType();
        };

        for (Function* f : code.luaWrapperFunctions) {
            updateState(f->returnValue);

            for (Variable* var : f->arguments) {
                updateState(var->type);
            }
        }

        if (hasCustomInMap || hasCustomInOptional || hasCustomInVector) {
            result += "namespace codegen {\n";

            // First, we need to define the declarations to handle all of the cases
            result += hasCustomInMap ? BakeCustomMapDeclaration : "";
            result += hasCustomInOptional ? BakeCustomOptionalDeclaration : "";
            result += hasCustomInVector ? BakeCustomVectorDeclaration : "";

            // Then we can define them properly
            result += hasCustomInMap ? BakeCustomMap : "";
            result += hasCustomInOptional ? BakeCustomOptional : "";
            result += hasCustomInVector ? BakeCustomVector : "";

            result += "} // namespace codegen\n\n";
        }

        result += "namespace codegen::lua {\n\n";

        for (Function* f : code.luaWrapperFunctions) {
            result += generateLuaFunction(f);
        }

        result += "} // namespace codegen::lua\n";
        return result;
    }

    std::string createClickableFileName(std::string filename) {
        for (size_t i = 0; i < filename.size(); i++) {
            if (filename[i] == '\\') {
                filename.insert(filename.begin() + i, '\\');
                i += 1;
            }
        }
        return filename;
    }
} // namespace

std::string generateResult(const Code& code) {
    assert(
        !code.structs.empty() ||
        !code.enums.empty() ||
        !code.luaWrapperFunctions.empty()
    );

    HeaderInfo info;
    std::string structs = generateStructsResult(code, info);
    std::string enums = generateEnumResult(code, info);
    std::string luaWrapper = generateLuaWrapperResult(code, info);

    std::string header;
    if (info.needsAny()) {
        header += "namespace codegen {\n";

        if (info.needsArrayifyFallback) {
            header += std::format("{}\n", ArrayifyFallback);
        }
        if (info.needsMappingFallback) {
            header += std::format("{}\n", MapFunctionFallback);
        }
        if (info.needsBakeEnumFallback) {
            header += std::format("{}\n", BakeEnumFallback);
        }
        if (info.needsToStringFallback) {
            header += std::format("{}\n", ToStringFallback);
        }
        if (info.needsFromStringFallback) {
            header += std::format("{}\n", FromStringFallback);
        }
        header += "} // namespace codegen\n";
    }

    // GCC has an overeager need to report an uninitialized variable when returning
    // a std::variant<std::string, ghoul::Dictionary> in a Lua function
    constexpr std::string_view GCCWarningStart =
#if defined(__GNUC__) && !defined(__clang__)
        "#pragma GCC diagnostic push\n"
        "#pragma GCC diagnostic ignored \"-Wmaybe-uninitialized\"\n";
#else
        "";
#endif
    constexpr std::string_view GCCWarningEnd =
#if defined(__GNUC__) && !defined(__clang__)
        "#pragma GCC diagnostic pop\n";
#else
        "";
#endif

    // The reversal in the arguments here is on purpose. The alternative to this would be
    // to first figure out whether we need to write the headers or not and that became
    // quite cumbersome. So instead we now mark whenever someone needs it and we prepend
    // it in this step
    return std::format(
        "{}{}\nnamespace {{\n{}{}{}{}\n}} // namespace\n{}",
        FileHeader, GCCWarningStart, header, enums, structs, luaWrapper, GCCWarningEnd
    );
}

Result handleFile(const std::filesystem::path& path) {
    std::ifstream file = std::ifstream(path);
    const std::string res = std::string(std::istreambuf_iterator<char>(file), {});
    file.close();


    Code code = parse(res, path);
    if (code.structs.empty() && code.enums.empty() && code.luaWrapperFunctions.empty()) {
        return Result::NotProcessed;
    }
    code.sourceFile = createClickableFileName(path.string());

    std::string content = generateResult(code);
    if (content.empty()) {
        return Result::NotProcessed;
    }

    std::filesystem::path destination = path;
    destination.replace_extension();
    destination.replace_filename(destination.filename().string() + "_codegen.cpp");

    bool shouldWriteFile = true;
    if (std::filesystem::exists(destination)) {
        std::ifstream f = std::ifstream(destination);
        const std::string prev = std::string(std::istreambuf_iterator<char>{f}, {});
        shouldWriteFile = (prev != content);
    }

    if (shouldWriteFile) {
        if constexpr (PreventFileChange) {
            throw CodegenError(std::format("File '{}' changed", path.filename()));
        }
    }

    std::filesystem::path debugDest = destination;
    debugDest.replace_extension();
    debugDest.replace_filename(debugDest.filename().string() + "_debug.cpp");

    if (shouldWriteFile || ShouldAlwaysWriteFiles) {
        std::cout << std::format("Processed file '{}'\n", path.filename());

        std::ofstream r(destination);
        r.write(content.data(), content.size());

        std::filesystem::remove(debugDest);
        return Result::Processed;
    }
    else {
        std::filesystem::remove(debugDest);
        return Result::Skipped;
    }
}
