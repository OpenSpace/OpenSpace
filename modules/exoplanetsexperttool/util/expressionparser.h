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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___EXPRESSIONPARSER___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___EXPRESSIONPARSER___H__

#include <functional>
#include <memory>
#include <string>
#include <vector>

namespace openspace::exoplanets {

enum class NodeType {
    Number,
    Variable,
    UnaryMinus,
    BinaryOp,
    Call
};

// A small arithmetic expression parser/evaluator, supporting +, -, *, /, unary minus,
// parentheses, numeric literals, named variables (resolved at evaluation time), and the
// functions sqrt, log, log10, abs, pow, min, max.
class Expression {
public:
    // Parses `text` into an expression. Check `isValid()` afterwards to see whether
    // parsing succeeded; if not, `errorMessage()` describes the problem.
    static Expression parse(const std::string& text);

    bool isValid() const;
    const std::string& errorMessage() const;

    // Evaluates the parsed expression. `resolveVariable` is called for every named
    // variable encountered and should return its numeric value (or NaN if unknown).
    // Only valid to call when `isValid()` is true.
    float evaluate(const std::function<float(const std::string&)>& resolveVariable) const;

    // A single node in the parsed expression tree, e.g. a literal, a variable
    // reference, or an operator/function applied to child nodes.
    struct Node {
        NodeType type;
        float number = 0.f;
        std::string name; // variable or function name
        char op = 0; // for BinaryOp
        std::vector<std::shared_ptr<Node>> children;
    };

private:
    Expression() = default;

    std::shared_ptr<Node> _root;
    bool _isValid = false;
    std::string _errorMessage;
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___EXPRESSIONPARSER___H__
