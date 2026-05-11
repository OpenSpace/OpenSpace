/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include "schema/assetschema.h"

#include <openspace/documentation/documentationengine.h>
#include <openspace/util/factorymanager.h>

using namespace openspace;

namespace {
    SchemaType parseDocumentation(const Documentation& documentation) {
        SchemaType type = {
            .name = documentation.name,
            .identifier = documentation.id,
            .description = documentation.description
        };

        for (const DocumentationEntry& p : documentation.entries) {
            SchemaMember entry = {
                .name = p.key,
                .type = p.verifier->type(),
                .isOptional = p.optional,
                .documentation = p.documentation
            };

            auto* tv = dynamic_cast<TableVerifier*>(p.verifier.get());
            auto* rv = dynamic_cast<ReferencingVerifier*>(p.verifier.get());

            if (rv) {
                const std::vector<Documentation>& doc = DocEng.documentations();
                auto it = std::find_if(
                    doc.begin(),
                    doc.end(),
                    [rv](const Documentation& d) { return d.id == rv->identifier; }
                );

                ghoul_assert(it != doc.end(), "Did not find reference");
                SchemaReference ref = {
                    .identifier = rv->identifier,
                    .name = it->name
                };
                entry.reference = std::move(ref);
            }
            else if (tv) {
                Documentation doc = { .entries = tv->documentations };

                // Since this is a table we need to recurse this function to extract data
                SchemaType tableDocs = parseDocumentation(doc);

                entry.members = tableDocs.members;
            }
            else {
                entry.description = p.verifier->documentation();
            }
            type.members.push_back(entry);
        }

        return type;
    }

    std::vector<SchemaCategory> loadCategories() {
        std::vector<Documentation> docs = DocEng.documentations();
        const std::vector<FactoryManager::FactoryInfo>& factories =
            FactoryManager::ref().factories();

        std::vector<SchemaCategory> result;
        for (const FactoryManager::FactoryInfo& factoryInfo : factories) {
            SchemaCategory category = {
                .name = factoryInfo.name
            };

            ghoul::TemplateFactoryBase* f = factoryInfo.factory.get();
            // Add documentation about base class
            auto factoryDoc = std::find_if(
                docs.begin(),
                docs.end(),
                [&factoryInfo](const Documentation& d) {
                    return d.name == factoryInfo.name;
                }
            );
            if (factoryDoc != docs.end()) {
                SchemaType type = parseDocumentation(*factoryDoc);
                category.types.push_back(type);
                // Remove documentation from list check at the end if all docs got put in
                docs.erase(factoryDoc);
            }
            else {
                SchemaType type = {
                    .name = factoryInfo.name,
                    .identifier = factoryInfo.name
                };
                category.types.push_back(type);
            }

            const std::vector<std::string>& registeredClasses = f->registeredClasses();
            for (const std::string& c : registeredClasses) {
                auto it = std::find_if(
                    docs.begin(),
                    docs.end(),
                    [&c](const Documentation& d) { return d.name == c; }
                );
                if (it != docs.end()) {
                    SchemaType type = parseDocumentation(*it);
                    category.types.push_back(type);
                    docs.erase(it);
                }
                else {
                    SchemaType type = {
                        .name = c,
                        .identifier = c
                    };
                    category.types.push_back(type);
                }
            }
            result.push_back(category);
        }


        SchemaCategory other = {
            .name = "Other"
        };
        for (const Documentation& doc : docs) {
            if (doc.id.empty()) {
                continue;
            }
            SchemaType type = parseDocumentation(doc);
            other.types.push_back(type);
        }
        result.push_back(other);
        return result;
    }
} // namespace

const AssetSchema& AssetSchema::instance() {
    static AssetSchema schema;
    return schema;
}

AssetSchema::AssetSchema()
    : _categories(loadCategories())
{}

const SchemaType* AssetSchema::findType(std::string_view identifier) const {
    for (const SchemaCategory& category : _categories) {
        for (const SchemaType& type : category.types) {
            if (type.identifier == identifier) {
                return &type;
            }
        }
    }
    return nullptr;
}

const SchemaCategory* AssetSchema::findCategoryByTypeId(std::string_view id) const {
    for (const SchemaCategory& category : _categories) {
        for (const SchemaType& type : category.types) {
            if (type.identifier == id) {
                return &category;
            }
        }
    }
    return nullptr;
}
