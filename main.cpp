
#include <cctype>
#include <cmath>
#include <iostream>
#include <optional>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

#ifdef LATTICE_USE_READLINE
#include <readline/history.h>
#include <readline/readline.h>
#endif

namespace lattice {

enum class TokenType {
    Number,
    Identifier,
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    Equal,
    End
};

struct Token {
    TokenType type;
    std::string lexeme;
    double number = 0.0;
};

class Lexer {
  public:
    explicit Lexer(std::string source) : source_(std::move(source)) {}

    std::vector<Token> tokenize() {
        std::vector<Token> tokens;
        while (!isAtEnd()) {
            skipWhitespace();
            if (isAtEnd()) {
                break;
            }
            char c = peek();
            if (std::isdigit(c) || (c == '.' && std::isdigit(peekNext()))) {
                tokens.push_back(number());
            } else if (std::isalpha(c) || c == '_') {
                tokens.push_back(identifier());
            } else {
                switch (c) {
                case '+':
                    tokens.push_back(simple(TokenType::Plus, "+"));
                    break;
                case '-':
                    tokens.push_back(simple(TokenType::Minus, "-"));
                    break;
                case '*':
                    tokens.push_back(simple(TokenType::Star, "*"));
                    break;
                case '/':
                    tokens.push_back(simple(TokenType::Slash, "/"));
                    break;
                case '(':
                    tokens.push_back(simple(TokenType::LParen, "("));
                    break;
                case ')':
                    tokens.push_back(simple(TokenType::RParen, ")"));
                    break;
                case '=':
                    tokens.push_back(simple(TokenType::Equal, "="));
                    break;
                default:
                    throw std::runtime_error(std::string("Unexpected character: ") + c);
                }
                advance();
            }
        }
        tokens.push_back(Token{TokenType::End, ""});
        return tokens;
    }

  private:
    bool isAtEnd() const { return current_ >= source_.size(); }

    char peek() const { return source_[current_]; }

    char peekNext() const {
        if (current_ + 1 >= source_.size()) {
            return '\0';
        }
        return source_[current_ + 1];
    }

    char advance() { return source_[current_++]; }

    void skipWhitespace() {
        while (!isAtEnd() && std::isspace(peek())) {
            advance();
        }
    }

    Token simple(TokenType type, const std::string &lexeme) {
        return Token{type, lexeme};
    }

    Token number() {
        std::string num;
        bool seenDot = false;
        while (!isAtEnd()) {
            char c = peek();
            if (std::isdigit(c)) {
                num += c;
            } else if (c == '.' && !seenDot) {
                seenDot = true;
                num += c;
            } else {
                break;
            }
            advance();
        }
        return Token{TokenType::Number, num, std::stod(num)};
    }

    Token identifier() {
        std::string id;
        while (!isAtEnd()) {
            char c = peek();
            if (std::isalnum(c) || c == '_') {
                id += c;
                advance();
            } else {
                break;
            }
        }
        return Token{TokenType::Identifier, id};
    }

    std::string source_;
    std::size_t current_ = 0;
};

class Parser {
  public:
    Parser(std::vector<Token> tokens, std::unordered_map<std::string, double> &env)
        : tokens_(std::move(tokens)), env_(env) {}

    double parse() {
        if (check(TokenType::Identifier) && checkNext(TokenType::Equal)) {
            auto name = advance().lexeme;
            advance(); // '='
            double value = expression();
            env_[name] = value;
            consume(TokenType::End, "Expected end of input after assignment.");
            return value;
        }
        double value = expression();
        consume(TokenType::End, "Expected end of input after expression.");
        return value;
    }

  private:
    bool check(TokenType type) const { return tokens_[current_].type == type; }

    bool checkNext(TokenType type) const {
        if (current_ + 1 >= tokens_.size()) {
            return false;
        }
        return tokens_[current_ + 1].type == type;
    }

    const Token &advance() {
        if (current_ < tokens_.size()) {
            current_++;
        }
        return tokens_[current_ - 1];
    }

    void consume(TokenType type, const std::string &message) {
        if (check(type)) {
            advance();
            return;
        }
        throw std::runtime_error(message);
    }

    double expression() {
        double value = term();
        while (check(TokenType::Plus) || check(TokenType::Minus)) {
            Token op = advance();
            double right = term();
            if (op.type == TokenType::Plus) {
                value += right;
            } else {
                value -= right;
            }
        }
        return value;
    }

    double term() {
        double value = factor();
        while (check(TokenType::Star) || check(TokenType::Slash)) {
            Token op = advance();
            double right = factor();
            if (op.type == TokenType::Star) {
                value *= right;
            } else {
                if (std::abs(right) < 1e-12) {
                    throw std::runtime_error("Division by zero.");
                }
                value /= right;
            }
        }
        return value;
    }

    double factor() {
        if (check(TokenType::Minus)) {
            advance();
            return -factor();
        }
        if (check(TokenType::Number)) {
            return advance().number;
        }
        if (check(TokenType::Identifier)) {
            auto name = advance().lexeme;
            auto it = env_.find(name);
            if (it == env_.end()) {
                throw std::runtime_error("Undefined variable: " + name);
            }
            return it->second;
        }
        if (check(TokenType::LParen)) {
            advance();
            double value = expression();
            consume(TokenType::RParen, "Expected ')'.");
            return value;
        }
        throw std::runtime_error("Unexpected token.");
    }

    std::vector<Token> tokens_;
    std::size_t current_ = 0;
    std::unordered_map<std::string, double> &env_;
};

std::optional<std::string> readInput(const std::string &prompt) {
#ifdef LATTICE_USE_READLINE
    // readline provides in-line editing and history navigation with arrow keys.
    char *line = ::readline(prompt.c_str());
    if (!line) {
        return std::nullopt;
    }
    std::string text(line);
    free(line);
    if (!text.empty()) {
        add_history(text.c_str());
    }
    return text;
#else
    std::cout << prompt << std::flush;
    std::string text;
    if (!std::getline(std::cin, text)) {
        return std::nullopt;
    }
    return text;
#endif
}

void repl() {
    std::unordered_map<std::string, double> env;
    std::string line;
    std::cout << "Lattice REPL (type 'quit' or 'exit' to leave)\n";
    while (true) {
        auto maybeLine = readInput("lattice> ");
        if (!maybeLine.has_value()) {
            break;
        }
        line = *maybeLine;
        if (line == "quit" || line == "exit") {
            break;
        }
        if (line.empty()) {
            continue;
        }
        try {
            Lexer lexer(line);
            auto tokens = lexer.tokenize();
            Parser parser(std::move(tokens), env);
            double result = parser.parse();
            env["ans"] = result;
            std::cout << result << '\n';
        } catch (const std::exception &ex) {
            std::cout << "Error: " << ex.what() << '\n';
        }
    }
    std::cout << "Goodbye.\n";
}

} // namespace lattice

int main() {
    lattice::repl();
    return 0;
}
