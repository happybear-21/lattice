
#include <cctype>
#include <cmath>
#include <complex>
#include <iostream>
#include <limits>
#include <numeric>
#include <optional>
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

const std::unordered_set<std::string> kReservedNames = {
    "pi", "tau", "e", "inf", "nan", "infj", "nanj"};

struct Value {
    std::complex<double> data;

    Value()
        : data(0.0, 0.0) {}
    explicit Value(double real)
        : data(real, 0.0) {}
    explicit Value(std::complex<double> value)
        : data(value) {}

    double real() const {
        return data.real();
    }

    double imag() const {
        return data.imag();
    }

    bool isZero() const {
        return data.real() == 0.0 && data.imag() == 0.0;
    }
};

Value add(const Value& a, const Value& b) {
    return Value(a.data + b.data);
}

Value subtract(const Value& a, const Value& b) {
    return Value(a.data - b.data);
}

Value multiply(const Value& a, const Value& b) {
    return Value(a.data * b.data);
}

Value divide(const Value& a, const Value& b) {
    if (b.isZero()) {
        throw std::runtime_error("Division by zero.");
    }
    return Value(a.data / b.data);
}

Value negate(const Value& v) {
    return Value(-v.data);
}

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
        tokens.reserve(source_.size());
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
                    throw std::runtime_error(
                        std::string("Unexpected character: ") + c);
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
    Parser(std::vector<Token> tokens,
           std::unordered_map<std::string, Value>& env)
        : tokens_(std::move(tokens))
        , env_(env) {}

    Value parse() {
        if (check(TokenType::Identifier) && checkNext(TokenType::Equal)) {
            auto name = advance().lexeme;
            if (kReservedNames.count(name)) {
                throw std::runtime_error("Cannot assign to constant " + name);
            }
            advance();
            Value value = expression();
            env_[name] = value;
            consume(TokenType::End, "Expected end of input after assignment.");
            return value;
        }
        Value value = expression();
        consume(TokenType::End, "Expected end of input after expression.");
        return value;
    }

  private:
    bool isInteger(double value) const {
        return std::isfinite(value) && std::floor(value) == value;
    }

    double requireReal(const std::string& name, const Value& v) const {
        if (v.imag() != 0.0) {
            throw std::runtime_error(name + " expects real arguments");
        }
        return v.real();
    }

    long long asInteger(const std::string& name, const Value& v) const {
        double value = requireReal(name, v);
        if (!isInteger(value)) {
            throw std::runtime_error(name + " expects integer arguments");
        }
        return static_cast<long long>(value);
    }

    void expectCount(const std::string& name, std::size_t expected,
                     std::size_t actual) {
        if (expected != actual) {
            throw std::runtime_error("Function " + name + " expects " +
                                     std::to_string(expected) + " argument(s)");
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

    long long foldGcd(const std::vector<Value>& args) {
        if (args.empty()) {
            throw std::runtime_error("gcd expects at least one argument");
        }
        long long g = std::abs(asInteger("gcd", args[0]));
        for (std::size_t i = 1; i < args.size(); ++i) {
            g = std::gcd(g, std::abs(asInteger("gcd", args[i])));
        }
        return g;
    }

    long long foldLcm(const std::vector<Value>& args) {
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

    Value applyFunction(const std::string& name,
                        const std::vector<Value>& args) {
        constexpr double pi = 3.14159265358979323846;
        if (name == "ceil") {
            expectCount(name, 1, args.size());
            return Value(std::ceil(requireReal(name, args[0])));
        }
        if (name == "floor") {
            expectCount(name, 1, args.size());
            return Value(std::floor(requireReal(name, args[0])));
        }
        if (name == "fabs") {
            expectCount(name, 1, args.size());
            return Value(std::fabs(requireReal(name, args[0])));
        }
        if (name == "trunc") {
            expectCount(name, 1, args.size());
            return Value(std::trunc(requireReal(name, args[0])));
        }
        if (name == "factorial") {
            expectCount(name, 1, args.size());
            long long n = asInteger(name, args[0]);
            if (n < 0) {
                throw std::runtime_error(
                    "factorial expects a non-negative integer");
            }
            double result = 1.0;
            for (long long i = 2; i <= n; ++i) {
                result *= static_cast<double>(i);
            }
            return Value(result);
        }
        if (name == "gcd") {
            return Value(static_cast<double>(foldGcd(args)));
        }
        if (name == "lcm") {
            return Value(static_cast<double>(foldLcm(args)));
        }
        if (name == "comb") {
            expectCount(name, 2, args.size());
            return Value(
                binomial(asInteger(name, args[0]), asInteger(name, args[1])));
        }
        if (name == "perm") {
            expectCount(name, 2, args.size());
            return Value(permutations(asInteger(name, args[0]),
                                      asInteger(name, args[1])));
        }
        if (name == "isqrt") {
            expectCount(name, 1, args.size());
            long long n = asInteger(name, args[0]);
            if (n < 0) {
                throw std::runtime_error("isqrt expects non-negative integer");
            }
            long long r =
                static_cast<long long>(std::sqrt(static_cast<double>(n)));
            while ((r + 1) * (r + 1) <= n) {
                ++r;
            }
            while (r * r > n) {
                --r;
            }
            return Value(static_cast<double>(r));
        }
        if (name == "fmod") {
            expectCount(name, 2, args.size());
            return Value(std::fmod(requireReal(name, args[0]),
                                   requireReal(name, args[1])));
        }
        if (name == "remainder") {
            expectCount(name, 2, args.size());
            return Value(std::remainder(requireReal(name, args[0]),
                                        requireReal(name, args[1])));
        }
        if (name == "fma") {
            expectCount(name, 3, args.size());
            return Value(std::fma(requireReal(name, args[0]),
                                  requireReal(name, args[1]),
                                  requireReal(name, args[2])));
        }
        if (name == "copysign") {
            expectCount(name, 2, args.size());
            return Value(std::copysign(requireReal(name, args[0]),
                                       requireReal(name, args[1])));
        }
        if (name == "modf") {
            expectCount(name, 1, args.size());
            double intPart = 0.0;
            double frac = std::modf(requireReal(name, args[0]), &intPart);
            env_["modf_int"] = Value(intPart);
            return Value(frac);
        }
        if (name == "pow") {
            expectCount(name, 2, args.size());
            return Value(std::pow(args[0].data, args[1].data));
        }
        if (name == "cbrt") {
            expectCount(name, 1, args.size());
            return Value(std::pow(args[0].data, 1.0 / 3.0));
        }
        if (name == "sqrt") {
            expectCount(name, 1, args.size());
            return Value(std::sqrt(args[0].data));
        }
        if (name == "exp") {
            expectCount(name, 1, args.size());
            return Value(std::exp(args[0].data));
        }
        if (name == "exp2") {
            expectCount(name, 1, args.size());
            return Value(
                std::pow(std::complex<double>(2.0, 0.0), args[0].data));
        }
        if (name == "expm1") {
            expectCount(name, 1, args.size());
            return Value(std::exp(args[0].data) -
                         std::complex<double>(1.0, 0.0));
        }
        if (name == "log") {
            if (args.size() == 1) {
                return Value(std::log(args[0].data));
            }
            if (args.size() == 2) {
                return Value(std::log(args[0].data) / std::log(args[1].data));
            }
            throw std::runtime_error("Function log expects 1 or 2 argument(s)");
        }
        if (name == "log1p") {
            expectCount(name, 1, args.size());
            return Value(
                std::log(std::complex<double>(1.0, 0.0) + args[0].data));
        }
        if (name == "log2") {
            expectCount(name, 1, args.size());
            return Value(std::log(args[0].data) /
                         std::log(std::complex<double>(2.0, 0.0)));
        }
        if (name == "log10") {
            expectCount(name, 1, args.size());
            return Value(std::log10(args[0].data));
        }
        if (name == "sin") {
            expectCount(name, 1, args.size());
            return Value(std::sin(args[0].data));
        }
        if (name == "cos") {
            expectCount(name, 1, args.size());
            return Value(std::cos(args[0].data));
        }
        if (name == "tan") {
            expectCount(name, 1, args.size());
            return Value(std::tan(args[0].data));
        }
        if (name == "asin") {
            expectCount(name, 1, args.size());
            return Value(std::asin(args[0].data));
        }
        if (name == "acos") {
            expectCount(name, 1, args.size());
            return Value(std::acos(args[0].data));
        }
        if (name == "atan") {
            expectCount(name, 1, args.size());
            return Value(std::atan(args[0].data));
        }
        if (name == "atan2") {
            expectCount(name, 2, args.size());
            return Value(std::atan2(requireReal(name, args[0]),
                                    requireReal(name, args[1])));
        }
        if (name == "sinh") {
            expectCount(name, 1, args.size());
            return Value(std::sinh(args[0].data));
        }
        if (name == "cosh") {
            expectCount(name, 1, args.size());
            return Value(std::cosh(args[0].data));
        }
        if (name == "tanh") {
            expectCount(name, 1, args.size());
            return Value(std::tanh(args[0].data));
        }
        if (name == "asinh") {
            expectCount(name, 1, args.size());
            return Value(std::asinh(args[0].data));
        }
        if (name == "acosh") {
            expectCount(name, 1, args.size());
            return Value(std::acosh(args[0].data));
        }
        if (name == "atanh") {
            expectCount(name, 1, args.size());
            return Value(std::atanh(args[0].data));
        }
        if (name == "degrees") {
            expectCount(name, 1, args.size());
            return Value(requireReal(name, args[0]) * (180.0 / pi));
        }
        if (name == "radians") {
            expectCount(name, 1, args.size());
            return Value(requireReal(name, args[0]) * (pi / 180.0));
        }
        if (name == "frexp") {
            expectCount(name, 1, args.size());
            int exponent = 0;
            double mantissa = std::frexp(requireReal(name, args[0]), &exponent);
            env_["frexp_exp"] = Value(static_cast<double>(exponent));
            return Value(mantissa);
        }
        if (name == "ldexp") {
            expectCount(name, 2, args.size());
            return Value(std::ldexp(requireReal(name, args[0]),
                                    asInteger(name, args[1])));
        }
        if (name == "isclose") {
            expectCount(name, 4, args.size());
            std::complex<double> a = args[0].data;
            std::complex<double> b = args[1].data;
            double rel = requireReal(name, args[2]);
            double absTol = requireReal(name, args[3]);
            double diff = std::abs(a - b);
            double limit =
                std::max(rel * std::max(std::abs(a), std::abs(b)), absTol);
            return Value(diff <= limit ? 1.0 : 0.0);
        }
        if (name == "isfinite") {
            expectCount(name, 1, args.size());
            auto c = args[0].data;
            return Value(
                std::isfinite(c.real()) && std::isfinite(c.imag()) ? 1.0 : 0.0);
        }
        if (name == "isinf") {
            expectCount(name, 1, args.size());
            auto c = args[0].data;
            return Value((std::isinf(c.real()) || std::isinf(c.imag())) ? 1.0
                                                                        : 0.0);
        }
        if (name == "isnan") {
            expectCount(name, 1, args.size());
            auto c = args[0].data;
            return Value((std::isnan(c.real()) || std::isnan(c.imag())) ? 1.0
                                                                        : 0.0);
        }
        if (name == "nextafter") {
            if (args.size() != 2 && args.size() != 3) {
                throw std::runtime_error(
                    "Function nextafter expects 2 or 3 argument(s)");
            }
            long long steps = 1;
            if (args.size() == 3) {
                steps = asInteger(name, args[2]);
                if (steps < 0) {
                    throw std::runtime_error(
                        "nextafter expects non-negative steps");
                }
            }
            double value = requireReal(name, args[0]);
            double target = requireReal(name, args[1]);
            for (long long i = 0; i < steps; ++i) {
                value = std::nextafter(value, target);
            }
            return Value(value);
        }
        if (name == "ulp") {
            expectCount(name, 1, args.size());
            double x = requireReal(name, args[0]);
            if (std::isnan(x)) {
                return Value(std::numeric_limits<double>::quiet_NaN());
            }
            if (std::isinf(x)) {
                return Value(std::numeric_limits<double>::infinity());
            }
            double next =
                std::nextafter(x, std::numeric_limits<double>::infinity());
            return Value(std::fabs(next - x));
        }
        if (name == "gamma") {
            expectCount(name, 1, args.size());
            return Value(std::tgamma(requireReal(name, args[0])));
        }
        if (name == "lgamma") {
            expectCount(name, 1, args.size());
            return Value(std::lgamma(requireReal(name, args[0])));
        }
        if (name == "erf") {
            expectCount(name, 1, args.size());
            return Value(std::erf(requireReal(name, args[0])));
        }
        if (name == "erfc") {
            expectCount(name, 1, args.size());
            return Value(std::erfc(requireReal(name, args[0])));
        }
        if (name == "dist") {
            if (args.size() % 2 != 0 || args.empty()) {
                throw std::runtime_error(
                    "dist expects an even number of coordinates");
            }
            std::size_t half = args.size() / 2;
            double sum = 0.0;
            for (std::size_t i = 0; i < half; ++i) {
                double d = requireReal(name, args[i]) -
                           requireReal(name, args[half + i]);
                sum += d * d;
            }
            return Value(std::sqrt(sum));
        }
        if (name == "fsum") {
            std::complex<double> sum(0.0, 0.0);
            for (const Value& v : args) {
                sum += v.data;
            }
            return Value(sum);
        }
        if (name == "hypot") {
            double result = 0.0;
            for (const Value& v : args) {
                result = std::hypot(result, requireReal(name, v));
            }
            return Value(result);
        }
        if (name == "prod") {
            if (args.empty()) {
                throw std::runtime_error("prod expects at least one argument");
            }
            std::complex<double> result = args[0].data;
            for (std::size_t i = 1; i < args.size(); ++i) {
                result *= args[i].data;
            }
            return Value(result);
        }
        if (name == "sumprod") {
            if (args.size() % 2 != 0 || args.empty()) {
                throw std::runtime_error(
                    "sumprod expects an even number of arguments");
            }
            std::size_t half = args.size() / 2;
            std::complex<double> sum(0.0, 0.0);
            for (std::size_t i = 0; i < half; ++i) {
                sum += args[i].data * args[half + i].data;
            }
            return Value(sum);
        }
        if (name == "phase") {
            expectCount(name, 1, args.size());
            return Value(std::arg(args[0].data));
        }
        if (name == "polar") {
            expectCount(name, 1, args.size());
            double r = std::abs(args[0].data);
            double phi = std::arg(args[0].data);
            return Value(std::complex<double>(r, phi));
        }
        if (name == "rect") {
            expectCount(name, 2, args.size());
            double r = requireReal(name, args[0]);
            double phi = requireReal(name, args[1]);
            return Value(std::polar(r, phi));
        }
        throw std::runtime_error("Unknown function: " + name);
    }

    std::vector<Value> parseArguments() {
        std::vector<Value> args;
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

    Value expression() {
        Value value = term();
        while (check(TokenType::Plus) || check(TokenType::Minus)) {
            Token op = advance();
            Value right = term();
            if (op.type == TokenType::Plus) {
                value = add(value, right);
            } else {
                value = subtract(value, right);
            }
        }
        return value;
    }

    Value term() {
        Value value = factor();
        while (check(TokenType::Star) || check(TokenType::Slash)) {
            Token op = advance();
            Value right = factor();
            if (op.type == TokenType::Star) {
                value = multiply(value, right);
            } else {
                value = divide(value, right);
            }
        }
        return value;
    }

    Value factor() {
        if (check(TokenType::Minus)) {
            advance();
            return negate(factor());
        }
        if (check(TokenType::Number)) {
            return Value(advance().number);
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
            Value value = expression();
            consume(TokenType::RParen, "Expected ')'.");
            return value;
        }
        throw std::runtime_error("Unexpected token.");
    }

    std::vector<Token> tokens_;
    std::size_t current_ = 0;
    std::unordered_map<std::string, Value>& env_;
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
    std::unordered_map<std::string, Value> env;
    env.reserve(256);
    env["pi"] = Value(3.14159265358979323846);
    env["tau"] = Value(6.28318530717958647692);
    env["e"] = Value(2.71828182845904523536);
    env["inf"] = Value(std::numeric_limits<double>::infinity());
    env["nan"] = Value(std::numeric_limits<double>::quiet_NaN());
    env["infj"] = Value(
        std::complex<double>(0.0, std::numeric_limits<double>::infinity()));
    env["nanj"] = Value(
        std::complex<double>(0.0, std::numeric_limits<double>::quiet_NaN()));
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
            Value result = parser.parse();
            env["ans"] = result;
            std::cout << result.data << '\n';
        } catch (const std::exception& ex) {
            std::cout << "Error: " << ex.what() << '\n';
        }
    }
    std::cout << "Goodbye.\n";
}

} // namespace lattice

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);
    lattice::repl();
    return 0;
}
