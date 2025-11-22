
#include <cctype>
#include <cmath>
#include <iostream>
#include <limits>
#include <optional>
#include <numeric>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#ifdef LATTICE_USE_READLINE
#include <readline/history.h>
#include <readline/readline.h>
#endif

namespace lattice {

const std::unordered_set<std::string> kReservedNames = {"pi", "tau", "e", "inf", "nan"};

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
    Comma,
    End
};

struct Token {
    TokenType type;
    std::string lexeme;
    double number = 0.0;
};

class Lexer {
  public:
    explicit Lexer(std::string source)
        : source_(std::move(source)) {}

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
                case ',':
                    tokens.push_back(simple(TokenType::Comma, ","));
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
    bool isAtEnd() const {
        return current_ >= source_.size();
    }

    char peek() const {
        return source_[current_];
    }

    char peekNext() const {
        if (current_ + 1 >= source_.size()) {
            return '\0';
        }
        return source_[current_ + 1];
    }

    char advance() {
        return source_[current_++];
    }

    void skipWhitespace() {
        while (!isAtEnd() && std::isspace(peek())) {
            advance();
        }
    }

    Token simple(TokenType type, const std::string& lexeme) {
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
    Parser(std::vector<Token> tokens, std::unordered_map<std::string, double>& env)
        : tokens_(std::move(tokens))
        , env_(env) {}

    double parse() {
        if (check(TokenType::Identifier) && checkNext(TokenType::Equal)) {
            auto name = advance().lexeme;
            if (kReservedNames.count(name)) {
                throw std::runtime_error("Cannot assign to constant " + name);
            }
            advance();
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
    bool isInteger(double value) const {
        return std::isfinite(value) && std::floor(value) == value;
    }

    long long asInteger(const std::string& name, double value) const {
        if (!isInteger(value)) {
            throw std::runtime_error(name + " expects integer arguments");
        }
        return static_cast<long long>(value);
    }

    void expectCount(const std::string& name, std::size_t expected, std::size_t actual) {
        if (expected != actual) {
            throw std::runtime_error("Function " + name + " expects " + std::to_string(expected) +
                                     " argument(s)");
        }
    }

    double binomial(long long n, long long k) {
        if (k < 0 || n < 0 || k > n) {
            throw std::runtime_error("comb expects 0 <= k <= n");
        }
        long long kk = std::min(k, n - k);
        double result = 1.0;
        for (long long i = 1; i <= kk; ++i) {
            result *= static_cast<double>(n - kk + i);
            result /= static_cast<double>(i);
        }
        return result;
    }

    double permutations(long long n, long long k) {
        if (k < 0 || n < 0 || k > n) {
            throw std::runtime_error("perm expects 0 <= k <= n");
        }
        double result = 1.0;
        for (long long i = 0; i < k; ++i) {
            result *= static_cast<double>(n - i);
        }
        return result;
    }

    long long foldGcd(const std::vector<double>& args) {
        if (args.empty()) {
            throw std::runtime_error("gcd expects at least one argument");
        }
        long long g = std::abs(asInteger("gcd", args[0]));
        for (std::size_t i = 1; i < args.size(); ++i) {
            g = std::gcd(g, std::abs(asInteger("gcd", args[i])));
        }
        return g;
    }

    long long foldLcm(const std::vector<double>& args) {
        if (args.empty()) {
            throw std::runtime_error("lcm expects at least one argument");
        }
        long long l = std::abs(asInteger("lcm", args[0]));
        for (std::size_t i = 1; i < args.size(); ++i) {
            long long v = std::abs(asInteger("lcm", args[i]));
            l = std::lcm(l, v);
        }
        return l;
    }

    double applyFunction(const std::string& name, const std::vector<double>& args) {
        constexpr double pi = 3.14159265358979323846;
        if (name == "ceil") {
            expectCount(name, 1, args.size());
            return std::ceil(args[0]);
        }
        if (name == "floor") {
            expectCount(name, 1, args.size());
            return std::floor(args[0]);
        }
        if (name == "fabs") {
            expectCount(name, 1, args.size());
            return std::fabs(args[0]);
        }
        if (name == "trunc") {
            expectCount(name, 1, args.size());
            return std::trunc(args[0]);
        }
        if (name == "factorial") {
            expectCount(name, 1, args.size());
            long long n = asInteger(name, args[0]);
            if (n < 0) {
                throw std::runtime_error("factorial expects a non-negative integer");
            }
            double result = 1.0;
            for (long long i = 2; i <= n; ++i) {
                result *= static_cast<double>(i);
            }
            return result;
        }
        if (name == "gcd") {
            return static_cast<double>(foldGcd(args));
        }
        if (name == "lcm") {
            return static_cast<double>(foldLcm(args));
        }
        if (name == "comb") {
            expectCount(name, 2, args.size());
            return binomial(asInteger(name, args[0]), asInteger(name, args[1]));
        }
        if (name == "perm") {
            expectCount(name, 2, args.size());
            return permutations(asInteger(name, args[0]), asInteger(name, args[1]));
        }
        if (name == "isqrt") {
            expectCount(name, 1, args.size());
            long long n = asInteger(name, args[0]);
            if (n < 0) {
                throw std::runtime_error("isqrt expects non-negative integer");
            }
            long long r = static_cast<long long>(std::sqrt(static_cast<double>(n)));
            while ((r + 1) * (r + 1) <= n) {
                ++r;
            }
            while (r * r > n) {
                --r;
            }
            return static_cast<double>(r);
        }
        if (name == "fmod") {
            expectCount(name, 2, args.size());
            return std::fmod(args[0], args[1]);
        }
        if (name == "remainder") {
            expectCount(name, 2, args.size());
            return std::remainder(args[0], args[1]);
        }
        if (name == "fma") {
            expectCount(name, 3, args.size());
            return std::fma(args[0], args[1], args[2]);
        }
        if (name == "copysign") {
            expectCount(name, 2, args.size());
            return std::copysign(args[0], args[1]);
        }
        if (name == "modf") {
            expectCount(name, 1, args.size());
            double intPart = 0.0;
            double frac = std::modf(args[0], &intPart);
            env_["modf_int"] = intPart;
            return frac;
        }
        if (name == "pow") {
            expectCount(name, 2, args.size());
            return std::pow(args[0], args[1]);
        }
        if (name == "cbrt") {
            expectCount(name, 1, args.size());
            return std::cbrt(args[0]);
        }
        if (name == "sqrt") {
            expectCount(name, 1, args.size());
            return std::sqrt(args[0]);
        }
        if (name == "exp") {
            expectCount(name, 1, args.size());
            return std::exp(args[0]);
        }
        if (name == "exp2") {
            expectCount(name, 1, args.size());
            return std::exp2(args[0]);
        }
        if (name == "expm1") {
            expectCount(name, 1, args.size());
            return std::expm1(args[0]);
        }
        if (name == "log") {
            if (args.size() == 1) {
                return std::log(args[0]);
            }
            if (args.size() == 2) {
                return std::log(args[0]) / std::log(args[1]);
            }
            throw std::runtime_error("Function log expects 1 or 2 argument(s)");
        }
        if (name == "log1p") {
            expectCount(name, 1, args.size());
            return std::log1p(args[0]);
        }
        if (name == "log2") {
            expectCount(name, 1, args.size());
            return std::log2(args[0]);
        }
        if (name == "log10") {
            expectCount(name, 1, args.size());
            return std::log10(args[0]);
        }
        if (name == "sin") {
            expectCount(name, 1, args.size());
            return std::sin(args[0]);
        }
        if (name == "cos") {
            expectCount(name, 1, args.size());
            return std::cos(args[0]);
        }
        if (name == "tan") {
            expectCount(name, 1, args.size());
            return std::tan(args[0]);
        }
        if (name == "asin") {
            expectCount(name, 1, args.size());
            return std::asin(args[0]);
        }
        if (name == "acos") {
            expectCount(name, 1, args.size());
            return std::acos(args[0]);
        }
        if (name == "atan") {
            expectCount(name, 1, args.size());
            return std::atan(args[0]);
        }
        if (name == "atan2") {
            expectCount(name, 2, args.size());
            return std::atan2(args[0], args[1]);
        }
        if (name == "sinh") {
            expectCount(name, 1, args.size());
            return std::sinh(args[0]);
        }
        if (name == "cosh") {
            expectCount(name, 1, args.size());
            return std::cosh(args[0]);
        }
        if (name == "tanh") {
            expectCount(name, 1, args.size());
            return std::tanh(args[0]);
        }
        if (name == "asinh") {
            expectCount(name, 1, args.size());
            return std::asinh(args[0]);
        }
        if (name == "acosh") {
            expectCount(name, 1, args.size());
            return std::acosh(args[0]);
        }
        if (name == "atanh") {
            expectCount(name, 1, args.size());
            return std::atanh(args[0]);
        }
        if (name == "degrees") {
            expectCount(name, 1, args.size());
            return args[0] * (180.0 / pi);
        }
        if (name == "radians") {
            expectCount(name, 1, args.size());
            return args[0] * (pi / 180.0);
        }
        if (name == "frexp") {
            expectCount(name, 1, args.size());
            int exponent = 0;
            double mantissa = std::frexp(args[0], &exponent);
            env_["frexp_exp"] = static_cast<double>(exponent);
            return mantissa;
        }
        if (name == "ldexp") {
            expectCount(name, 2, args.size());
            return std::ldexp(args[0], asInteger(name, args[1]));
        }
        if (name == "isclose") {
            expectCount(name, 4, args.size());
            double a = args[0];
            double b = args[1];
            double rel = args[2];
            double absTol = args[3];
            double diff = std::fabs(a - b);
            double limit = std::max(rel * std::max(std::fabs(a), std::fabs(b)), absTol);
            return diff <= limit ? 1.0 : 0.0;
        }
        if (name == "isfinite") {
            expectCount(name, 1, args.size());
            return std::isfinite(args[0]) ? 1.0 : 0.0;
        }
        if (name == "isinf") {
            expectCount(name, 1, args.size());
            return std::isinf(args[0]) ? 1.0 : 0.0;
        }
        if (name == "isnan") {
            expectCount(name, 1, args.size());
            return std::isnan(args[0]) ? 1.0 : 0.0;
        }
        if (name == "nextafter") {
            if (args.size() != 2 && args.size() != 3) {
                throw std::runtime_error("Function nextafter expects 2 or 3 argument(s)");
            }
            long long steps = 1;
            if (args.size() == 3) {
                steps = asInteger(name, args[2]);
                if (steps < 0) {
                    throw std::runtime_error("nextafter expects non-negative steps");
                }
            }
            double value = args[0];
            for (long long i = 0; i < steps; ++i) {
                value = std::nextafter(value, args[1]);
            }
            return value;
        }
        if (name == "ulp") {
            expectCount(name, 1, args.size());
            double x = args[0];
            if (std::isnan(x)) {
                return std::numeric_limits<double>::quiet_NaN();
            }
            if (std::isinf(x)) {
                return std::numeric_limits<double>::infinity();
            }
            double next = std::nextafter(x, std::numeric_limits<double>::infinity());
            return std::fabs(next - x);
        }
        if (name == "gamma") {
            expectCount(name, 1, args.size());
            return std::tgamma(args[0]);
        }
        if (name == "lgamma") {
            expectCount(name, 1, args.size());
            return std::lgamma(args[0]);
        }
        if (name == "erf") {
            expectCount(name, 1, args.size());
            return std::erf(args[0]);
        }
        if (name == "erfc") {
            expectCount(name, 1, args.size());
            return std::erfc(args[0]);
        }
        if (name == "dist") {
            if (args.size() % 2 != 0 || args.empty()) {
                throw std::runtime_error("dist expects an even number of coordinates");
            }
            std::size_t half = args.size() / 2;
            double sum = 0.0;
            for (std::size_t i = 0; i < half; ++i) {
                double d = args[i] - args[half + i];
                sum += d * d;
            }
            return std::sqrt(sum);
        }
        if (name == "fsum") {
            double sum = 0.0;
            for (double v : args) {
                sum += v;
            }
            return sum;
        }
        if (name == "hypot") {
            double result = 0.0;
            for (double v : args) {
                result = std::hypot(result, v);
            }
            return result;
        }
        if (name == "prod") {
            if (args.empty()) {
                throw std::runtime_error("prod expects at least one argument");
            }
            double result = args[0];
            for (std::size_t i = 1; i < args.size(); ++i) {
                result *= args[i];
            }
            return result;
        }
        if (name == "sumprod") {
            if (args.size() % 2 != 0 || args.empty()) {
                throw std::runtime_error("sumprod expects an even number of arguments");
            }
            std::size_t half = args.size() / 2;
            double sum = 0.0;
            for (std::size_t i = 0; i < half; ++i) {
                sum += args[i] * args[half + i];
            }
            return sum;
        }
        throw std::runtime_error("Unknown function: " + name);
    }

    std::vector<double> parseArguments() {
        std::vector<double> args;
        if (check(TokenType::RParen)) {
            return args;
        }
        args.push_back(expression());
        while (check(TokenType::Comma)) {
            advance();
            args.push_back(expression());
        }
        return args;
    }

    bool check(TokenType type) const {
        return tokens_[current_].type == type;
    }

    bool checkNext(TokenType type) const {
        if (current_ + 1 >= tokens_.size()) {
            return false;
        }
        return tokens_[current_ + 1].type == type;
    }

    const Token& advance() {
        if (current_ < tokens_.size()) {
            current_++;
        }
        return tokens_[current_ - 1];
    }

    void consume(TokenType type, const std::string& message) {
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
            if (check(TokenType::LParen)) {
                advance();
                auto args = parseArguments();
                consume(TokenType::RParen, "Expected ')'.");
                return applyFunction(name, args);
            }
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
    std::unordered_map<std::string, double>& env_;
};

std::optional<std::string> readInput(const std::string& prompt) {
#ifdef LATTICE_USE_READLINE
    char* line = ::readline(prompt.c_str());
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
    env["pi"] = 3.14159265358979323846;
    env["tau"] = 6.28318530717958647692;
    env["e"] = 2.71828182845904523536;
    env["inf"] = std::numeric_limits<double>::infinity();
    env["nan"] = std::numeric_limits<double>::quiet_NaN();
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
        } catch (const std::exception& ex) {
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
