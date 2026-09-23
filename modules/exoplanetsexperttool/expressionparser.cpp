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

#include <modules/exoplanetsexperttool/expressionparser.h>

#include <algorithm>
#include <cctype>
#include <cmath>
#include <format>
#include <limits>
#include <stdexcept>
#include <vector>

namespace {

    enum class TokenType {
        Number,
        Identifier,
        Plus,
        Minus,
        Star,
        Slash,
        LParen,
        RParen,
        Comma,
        End
    };

    struct Token {
        TokenType type;
        std::string text;
        float number = 0.f;
    };

    std::vector<Token> tokenize(const std::string& text) {
        std::vector<Token> tokens;
        size_t i = 0;
        while (i < text.size()) {
            const char c = text[i];
            if (std::isspace(static_cast<unsigned char>(c))) {
                i++;
                continue;
            }
            if (std::isdigit(static_cast<unsigned char>(c)) || c == '.') {
                size_t start = i;
                while (i < text.size() &&
                       (std::isdigit(static_cast<unsigned char>(text[i])) || text[i] == '.'))
                {
                    i++;
                }
                const std::string numberText = text.substr(start, i - start);
                Token token;
                token.type = TokenType::Number;
                token.text = numberText;
                token.number = std::stof(numberText);
                tokens.push_back(token);
                continue;
            }
            if (std::isalpha(static_cast<unsigned char>(c)) || c == '_') {
                size_t start = i;
                while (i < text.size() &&
                       (std::isalnum(static_cast<unsigned char>(text[i])) || text[i] == '_'))
                {
                    i++;
                }
                tokens.push_back({
                    TokenType::Identifier,
                    text.substr(start, i - start),
                    0.f
                });
                continue;
            }
            switch (c) {
                case '+': tokens.push_back({ TokenType::Plus, "+", 0.f }); break;
                case '-': tokens.push_back({ TokenType::Minus, "-", 0.f }); break;
                case '*': tokens.push_back({ TokenType::Star, "*", 0.f }); break;
                case '/': tokens.push_back({ TokenType::Slash, "/", 0.f }); break;
                case '(': tokens.push_back({ TokenType::LParen, "(", 0.f }); break;
                case ')': tokens.push_back({ TokenType::RParen, ")", 0.f }); break;
                case ',': tokens.push_back({ TokenType::Comma, ",", 0.f }); break;
                default:
                    throw std::runtime_error(std::format("Unexpected character '{}'", c));
            }
            i++;
        }
        tokens.push_back({ TokenType::End, "", 0.f });
        return tokens;
    }

} // namespace

namespace openspace::exoplanets {

namespace {

using Node = Expression::Node;
using NodePtr = std::shared_ptr<Node>;

// Number of arguments expected for each supported function, used both to validate
// calls while parsing and to know how to evaluate them.
int functionArgCount(const std::string& name) {
    if (name == "sqrt" || name == "log" || name == "log10" || name == "abs") {
        return 1;
    }
    if (name == "pow" || name == "min" || name == "max") {
        return 2;
    }
    return -1; // Unknown function
}

// Recursive-descent parsing, threaded through a shared token list and read position
// rather than wrapped in its own parser class/object.
void advanceToken(const std::vector<Token>& tokens, size_t& pos) {
    if (pos + 1 < tokens.size()) {
        pos++;
    }
}

void expectToken(const std::vector<Token>& tokens, size_t& pos, TokenType type,
                  const std::string& message)
{
    if (tokens[pos].type != type) {
        throw std::runtime_error(message);
    }
    advanceToken(tokens, pos);
}

NodePtr parseAdditive(const std::vector<Token>& tokens, size_t& pos);

NodePtr parsePrimary(const std::vector<Token>& tokens, size_t& pos) {
    const Token token = tokens[pos];

    if (token.type == TokenType::Number) {
        advanceToken(tokens, pos);
        auto node = std::make_shared<Node>();
        node->type = NodeType::Number;
        node->number = token.number;
        return node;
    }

    if (token.type == TokenType::LParen) {
        advanceToken(tokens, pos);
        NodePtr node = parseAdditive(tokens, pos);
        expectToken(tokens, pos, TokenType::RParen, "Expected closing parenthesis");
        return node;
    }

    if (token.type == TokenType::Identifier) {
        advanceToken(tokens, pos);
        if (tokens[pos].type == TokenType::LParen) {
            advanceToken(tokens, pos);
            std::vector<NodePtr> args;
            if (tokens[pos].type != TokenType::RParen) {
                args.push_back(parseAdditive(tokens, pos));
                while (tokens[pos].type == TokenType::Comma) {
                    advanceToken(tokens, pos);
                    args.push_back(parseAdditive(tokens, pos));
                }
            }
            expectToken(
                tokens, pos, TokenType::RParen,
                "Expected closing parenthesis in function call"
            );

            const int expectedArgs = functionArgCount(token.text);
            if (expectedArgs < 0) {
                throw std::runtime_error(std::format("Unknown function '{}'", token.text));
            }
            if (static_cast<int>(args.size()) != expectedArgs) {
                throw std::runtime_error(std::format(
                    "Function '{}' expects {} argument(s), got {}",
                    token.text, expectedArgs, args.size()
                ));
            }

            auto node = std::make_shared<Node>();
            node->type = NodeType::Call;
            node->name = token.text;
            node->children = std::move(args);
            return node;
        }

        auto node = std::make_shared<Node>();
        node->type = NodeType::Variable;
        node->name = token.text;
        return node;
    }

    throw std::runtime_error("Expected a number, variable, or '('");
}

NodePtr parseUnary(const std::vector<Token>& tokens, size_t& pos) {
    if (tokens[pos].type == TokenType::Minus) {
        advanceToken(tokens, pos);
        auto node = std::make_shared<Node>();
        node->type = NodeType::UnaryMinus;
        node->children = { parseUnary(tokens, pos) };
        return node;
    }
    if (tokens[pos].type == TokenType::Plus) {
        advanceToken(tokens, pos);
        return parseUnary(tokens, pos);
    }
    return parsePrimary(tokens, pos);
}

NodePtr parseMultiplicative(const std::vector<Token>& tokens, size_t& pos) {
    NodePtr left = parseUnary(tokens, pos);
    while (tokens[pos].type == TokenType::Star || tokens[pos].type == TokenType::Slash) {
        const char op = tokens[pos].type == TokenType::Star ? '*' : '/';
        advanceToken(tokens, pos);
        NodePtr right = parseUnary(tokens, pos);
        auto node = std::make_shared<Node>();
        node->type = NodeType::BinaryOp;
        node->op = op;
        node->children = { left, right };
        left = node;
    }
    return left;
}

NodePtr parseAdditive(const std::vector<Token>& tokens, size_t& pos) {
    NodePtr left = parseMultiplicative(tokens, pos);
    while (tokens[pos].type == TokenType::Plus || tokens[pos].type == TokenType::Minus) {
        const char op = tokens[pos].type == TokenType::Plus ? '+' : '-';
        advanceToken(tokens, pos);
        NodePtr right = parseMultiplicative(tokens, pos);
        auto node = std::make_shared<Node>();
        node->type = NodeType::BinaryOp;
        node->op = op;
        node->children = { left, right };
        left = node;
    }
    return left;
}

NodePtr parseExpressionTokens(const std::vector<Token>& tokens) {
    size_t pos = 0;
    NodePtr node = parseAdditive(tokens, pos);
    expectToken(tokens, pos, TokenType::End, "Unexpected trailing characters");
    return node;
}

float evaluateNode(const Node& node,
                    const std::function<float(const std::string&)>& resolveVariable)
{
    switch (node.type) {
        case NodeType::Number:
            return node.number;
        case NodeType::Variable:
            return resolveVariable(node.name);
        case NodeType::UnaryMinus:
            return -evaluateNode(*node.children[0], resolveVariable);
        case NodeType::BinaryOp: {
            const float left = evaluateNode(*node.children[0], resolveVariable);
            const float right = evaluateNode(*node.children[1], resolveVariable);
            switch (node.op) {
                case '+': return left + right;
                case '-': return left - right;
                case '*': return left * right;
                case '/': return left / right;
                default:  return std::numeric_limits<float>::quiet_NaN();
            }
        }
        case NodeType::Call: {
            if (node.name == "sqrt") {
                return std::sqrt(evaluateNode(*node.children[0], resolveVariable));
            }
            if (node.name == "log") {
                return std::log(evaluateNode(*node.children[0], resolveVariable));
            }
            if (node.name == "log10") {
                return std::log10(evaluateNode(*node.children[0], resolveVariable));
            }
            if (node.name == "abs") {
                return std::abs(evaluateNode(*node.children[0], resolveVariable));
            }
            if (node.name == "pow") {
                return std::pow(
                    evaluateNode(*node.children[0], resolveVariable),
                    evaluateNode(*node.children[1], resolveVariable)
                );
            }
            if (node.name == "min") {
                return std::min(
                    evaluateNode(*node.children[0], resolveVariable),
                    evaluateNode(*node.children[1], resolveVariable)
                );
            }
            if (node.name == "max") {
                return std::max(
                    evaluateNode(*node.children[0], resolveVariable),
                    evaluateNode(*node.children[1], resolveVariable)
                );
            }
            return std::numeric_limits<float>::quiet_NaN();
        }
    }
    return std::numeric_limits<float>::quiet_NaN();
}

} // namespace

Expression Expression::parse(const std::string& text) {
    Expression expression;
    try {
        const std::vector<Token> tokens = tokenize(text);
        expression._root = parseExpressionTokens(tokens);
        expression._isValid = true;
    }
    catch (const std::runtime_error& e) {
        expression._isValid = false;
        expression._errorMessage = e.what();
    }
    return expression;
}

bool Expression::isValid() const {
    return _isValid;
}

const std::string& Expression::errorMessage() const {
    return _errorMessage;
}

float Expression::evaluate(
                          const std::function<float(const std::string&)>& resolveVariable
) const {
    return evaluateNode(*_root, resolveVariable);
}

} // namespace openspace::exoplanets
