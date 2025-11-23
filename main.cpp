
#include <cctype>
#include <cmath>
#include <complex>
#include <iostream>
#include <limits>
#include <numeric>
#include <optional>
#include <sstream>
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
    "pi", "tau", "e", "inf", "nan", "infj", "nanj", "print", "input"};

const char* kHelpText =
    "Lattice quick help:\n"
    "  - Scalars: default numbers are real; complex results print as (a,b).\n"
    "  - Complex constructor: complex(real_part, imag_part)\n"
    "  - Vectors: [1, 2, 3], elementwise + -; * allowed only scalar*vector.\n"
    "    Vector helpers: zeros(n), ones(n), copy(v), dot(a, b) (alias "
    "scalar).\n"
    "  - Assignment: x = 5, v = [1,2]. Last value in ans.\n"
    "  - Print: print expr\n"
    "  - Input: input name   (prompts on the next line)\n"
    "  - Constants: pi, tau, e, inf, nan, infj, nanj\n"
    "  - Functions: trig (sin/cos/tan...), exp/log variants, factorial, gcd, "
    "lcm, comb, perm, dist, hypot, fsum, prod, sumprod, phase, polar, rect, "
    "and more. See source for full list.\n"
    "Type quit or exit to leave.\n";

struct Value {
    enum class Kind { Scalar, Vector };

    Kind kind = Kind::Scalar;
    std::complex<double> data;
    std::vector<std::complex<double>> vec;

    Value()
        : kind(Kind::Scalar)
        , data(0.0, 0.0) {}
    explicit Value(double real)
        : kind(Kind::Scalar)
        , data(real, 0.0) {}
    explicit Value(std::complex<double> value)
        : kind(Kind::Scalar)
        , data(value) {}
    explicit Value(std::vector<std::complex<double>> values)
        : kind(Kind::Vector)
        , data(0.0, 0.0)
        , vec(std::move(values)) {}

    bool isScalar() const {
        return kind == Kind::Scalar;
    }

    bool isVector() const {
        return kind == Kind::Vector;
    }

    double real() const {
        if (!isScalar()) {
            throw std::runtime_error("Expected scalar value");
        }
        return data.real();
    }

    double imag() const {
        if (!isScalar()) {
            throw std::runtime_error("Expected scalar value");
        }
        return data.imag();
    }

    bool isZero() const {
        if (!isScalar()) {
            throw std::runtime_error("Expected scalar value");
        }
        return data.real() == 0.0 && data.imag() == 0.0;
    }
};

std::string complexToString(const std::complex<double>& c) {
    std::ostringstream oss;
    if (c.imag() == 0.0) {
        oss << c.real();
        return oss.str();
    }
    oss << c;
    return oss.str();
}

std::string toString(const Value& v) {
    if (v.isScalar()) {
        return complexToString(v.data);
    }
    std::ostringstream oss;
    oss << '[';
    for (std::size_t i = 0; i < v.vec.size(); ++i) {
        if (i != 0) {
            oss << ", ";
        }
        oss << complexToString(v.vec[i]);
    }
    oss << ']';
    return oss.str();
}

void printValue(const Value& v) {
    std::cout << toString(v) << '\n';
}

bool isZero(const std::complex<double>& c) {
    return c.real() == 0.0 && c.imag() == 0.0;
}

template <typename Op>
Value elementwiseOp(const Value& a, const Value& b, Op op,
                    const std::string& name) {
    if (a.isScalar() && b.isScalar()) {
        return Value(op(a.data, b.data));
    }
    if (a.isVector() && b.isScalar()) {
        std::vector<std::complex<double>> result = a.vec;
        for (auto& val : result) {
            val = op(val, b.data);
        }
        return Value(std::move(result));
    }
    if (a.isScalar() && b.isVector()) {
        std::vector<std::complex<double>> result = b.vec;
        for (auto& val : result) {
            val = op(a.data, val);
        }
        return Value(std::move(result));
    }
    if (a.isVector() && b.isVector()) {
        if (a.vec.size() != b.vec.size()) {
            throw std::runtime_error(name +
                                     " expects vectors of the same length");
        }
        std::vector<std::complex<double>> result;
        result.reserve(a.vec.size());
        for (std::size_t i = 0; i < a.vec.size(); ++i) {
            result.push_back(op(a.vec[i], b.vec[i]));
        }
        return Value(std::move(result));
    }
    throw std::runtime_error("Unsupported operands for " + name);
}

Value add(const Value& a, const Value& b) {
    return elementwiseOp(
        a, b, [](auto lhs, auto rhs) { return lhs + rhs; }, "addition");
}

Value subtract(const Value& a, const Value& b) {
    return elementwiseOp(
        a, b, [](auto lhs, auto rhs) { return lhs - rhs; }, "subtraction");
}

Value multiply(const Value& a, const Value& b) {
    if (a.isVector() && b.isVector()) {
        throw std::runtime_error(
            "Vector-vector '*' is not allowed; use dot(a, b) instead");
    }
    return elementwiseOp(
        a, b, [](auto lhs, auto rhs) { return lhs * rhs; }, "multiplication");
}

Value divide(const Value& a, const Value& b) {
    if (b.isScalar() && b.isZero()) {
        throw std::runtime_error("Division by zero.");
    }
    if (b.isVector()) {
        for (const auto& val : b.vec) {
            if (isZero(val)) {
                throw std::runtime_error("Division by zero.");
            }
        }
    }
    return elementwiseOp(
        a, b, [](auto lhs, auto rhs) { return lhs / rhs; }, "division");
}

Value negate(const Value& v) {
    if (v.isScalar()) {
        return Value(-v.data);
    }
    std::vector<std::complex<double>> result = v.vec;
    for (auto& entry : result) {
        entry = -entry;
    }
    return Value(std::move(result));
}

enum class TokenType {
    Number,
    Identifier,
    Print,
    Input,
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    LBracket,
    RBracket,
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
                case '[':
                    tokens.push_back(simple(TokenType::LBracket, "["));
                    break;
                case ']':
                    tokens.push_back(simple(TokenType::RBracket, "]"));
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
        if (id == "print") {
            return Token{TokenType::Print, id};
        }
        if (id == "input") {
            return Token{TokenType::Input, id};
        }
        return Token{TokenType::Identifier, id};
    }

    std::string source_;
    std::size_t current_ = 0;
};

struct ParseResult {
    Value value;
    bool suppressOutput = false;
};

std::optional<std::string> readInput(const std::string& prompt);

class Parser {
  public:
    Parser(std::vector<Token> tokens,
           std::unordered_map<std::string, Value>& env)
        : tokens_(std::move(tokens))
        , env_(env) {}

    ParseResult parse() {
        if (check(TokenType::Print)) {
            advance();
            Value value = expression();
            consume(TokenType::End, "Expected end of input after print.");
            printValue(value);
            return {value, true};
        }
        if (check(TokenType::Input)) {
            advance();
            if (!check(TokenType::Identifier)) {
                throw std::runtime_error(
                    "input expects an identifier destination");
            }
            auto nameToken = advance();
            if (kReservedNames.count(nameToken.lexeme)) {
                throw std::runtime_error("Cannot assign to constant " +
                                         nameToken.lexeme);
            }
            consume(TokenType::End, "Expected end of input after input.");
            auto line = readInput("input> ");
            if (!line.has_value()) {
                throw std::runtime_error("No input provided");
            }
            Lexer nestedLexer(*line);
            auto nestedTokens = nestedLexer.tokenize();
            Parser nestedParser(std::move(nestedTokens), env_);
            ParseResult nestedResult = nestedParser.parse();
            env_[nameToken.lexeme] = nestedResult.value;
            return {nestedResult.value, false};
        }
        if (check(TokenType::Identifier) && checkNext(TokenType::Equal)) {
            auto name = advance().lexeme;
            if (kReservedNames.count(name)) {
                throw std::runtime_error("Cannot assign to constant " + name);
            }
            advance();
            Value value = expression();
            env_[name] = value;
            consume(TokenType::End, "Expected end of input after assignment.");
            return {value, false};
        }
        Value value = expression();
        consume(TokenType::End, "Expected end of input after expression.");
        return {value, false};
    }

  private:
    const std::complex<double>& requireScalar(const std::string& name,
                                              const Value& v) const {
        if (!v.isScalar()) {
            throw std::runtime_error(name + " expects scalar arguments");
        }
        return v.data;
    }

    const std::vector<std::complex<double>>&
    requireVector(const std::string& name, const Value& v) const {
        if (!v.isVector()) {
            throw std::runtime_error(name + " expects vector arguments");
        }
        return v.vec;
    }

    bool isInteger(double value) const {
        return std::isfinite(value) && std::floor(value) == value;
    }

    double requireReal(const std::string& name, const Value& v) const {
        const std::complex<double>& scalar = requireScalar(name, v);
        if (scalar.imag() != 0.0) {
            throw std::runtime_error(name + " expects real arguments");
        }
        return scalar.real();
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
            return Value(std::pow(requireScalar(name, args[0]),
                                  requireScalar(name, args[1])));
        }
        if (name == "cbrt") {
            expectCount(name, 1, args.size());
            return Value(std::pow(requireScalar(name, args[0]), 1.0 / 3.0));
        }
        if (name == "sqrt") {
            expectCount(name, 1, args.size());
            return Value(std::sqrt(requireScalar(name, args[0])));
        }
        if (name == "exp") {
            expectCount(name, 1, args.size());
            return Value(std::exp(requireScalar(name, args[0])));
        }
        if (name == "exp2") {
            expectCount(name, 1, args.size());
            return Value(std::pow(std::complex<double>(2.0, 0.0),
                                  requireScalar(name, args[0])));
        }
        if (name == "expm1") {
            expectCount(name, 1, args.size());
            return Value(std::exp(requireScalar(name, args[0])) -
                         std::complex<double>(1.0, 0.0));
        }
        if (name == "log") {
            if (args.size() == 1) {
                return Value(std::log(requireScalar(name, args[0])));
            }
            if (args.size() == 2) {
                return Value(std::log(requireScalar(name, args[0])) /
                             std::log(requireScalar(name, args[1])));
            }
            throw std::runtime_error("Function log expects 1 or 2 argument(s)");
        }
        if (name == "log1p") {
            expectCount(name, 1, args.size());
            return Value(std::log(std::complex<double>(1.0, 0.0) +
                                  requireScalar(name, args[0])));
        }
        if (name == "log2") {
            expectCount(name, 1, args.size());
            return Value(std::log(requireScalar(name, args[0])) /
                         std::log(std::complex<double>(2.0, 0.0)));
        }
        if (name == "log10") {
            expectCount(name, 1, args.size());
            return Value(std::log10(requireScalar(name, args[0])));
        }
        if (name == "sin") {
            expectCount(name, 1, args.size());
            return Value(std::sin(requireScalar(name, args[0])));
        }
        if (name == "cos") {
            expectCount(name, 1, args.size());
            return Value(std::cos(requireScalar(name, args[0])));
        }
        if (name == "tan") {
            expectCount(name, 1, args.size());
            return Value(std::tan(requireScalar(name, args[0])));
        }
        if (name == "asin") {
            expectCount(name, 1, args.size());
            return Value(std::asin(requireScalar(name, args[0])));
        }
        if (name == "acos") {
            expectCount(name, 1, args.size());
            return Value(std::acos(requireScalar(name, args[0])));
        }
        if (name == "atan") {
            expectCount(name, 1, args.size());
            return Value(std::atan(requireScalar(name, args[0])));
        }
        if (name == "atan2") {
            expectCount(name, 2, args.size());
            return Value(std::atan2(requireReal(name, args[0]),
                                    requireReal(name, args[1])));
        }
        if (name == "sinh") {
            expectCount(name, 1, args.size());
            return Value(std::sinh(requireScalar(name, args[0])));
        }
        if (name == "cosh") {
            expectCount(name, 1, args.size());
            return Value(std::cosh(requireScalar(name, args[0])));
        }
        if (name == "tanh") {
            expectCount(name, 1, args.size());
            return Value(std::tanh(requireScalar(name, args[0])));
        }
        if (name == "asinh") {
            expectCount(name, 1, args.size());
            return Value(std::asinh(requireScalar(name, args[0])));
        }
        if (name == "acosh") {
            expectCount(name, 1, args.size());
            return Value(std::acosh(requireScalar(name, args[0])));
        }
        if (name == "atanh") {
            expectCount(name, 1, args.size());
            return Value(std::atanh(requireScalar(name, args[0])));
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
            std::complex<double> a = requireScalar(name, args[0]);
            std::complex<double> b = requireScalar(name, args[1]);
            double rel = requireReal(name, args[2]);
            double absTol = requireReal(name, args[3]);
            double diff = std::abs(a - b);
            double limit =
                std::max(rel * std::max(std::abs(a), std::abs(b)), absTol);
            return Value(diff <= limit ? 1.0 : 0.0);
        }
        if (name == "isfinite") {
            expectCount(name, 1, args.size());
            auto c = requireScalar(name, args[0]);
            return Value(
                std::isfinite(c.real()) && std::isfinite(c.imag()) ? 1.0 : 0.0);
        }
        if (name == "isinf") {
            expectCount(name, 1, args.size());
            auto c = requireScalar(name, args[0]);
            return Value((std::isinf(c.real()) || std::isinf(c.imag())) ? 1.0
                                                                        : 0.0);
        }
        if (name == "isnan") {
            expectCount(name, 1, args.size());
            auto c = requireScalar(name, args[0]);
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
                sum += requireScalar(name, v);
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
            std::complex<double> result = requireScalar(name, args[0]);
            for (std::size_t i = 1; i < args.size(); ++i) {
                result *= requireScalar(name, args[i]);
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
                sum += requireScalar(name, args[i]) *
                       requireScalar(name, args[half + i]);
            }
            return Value(sum);
        }
        if (name == "copy") {
            expectCount(name, 1, args.size());
            const auto& v = requireVector(name, args[0]);
            return Value(v);
        }
        if (name == "zeros") {
            expectCount(name, 1, args.size());
            long long n = asInteger(name, args[0]);
            if (n < 0) {
                throw std::runtime_error("zeros expects non-negative length");
            }
            return Value(std::vector<std::complex<double>>(
                static_cast<std::size_t>(n), std::complex<double>(0.0, 0.0)));
        }
        if (name == "ones") {
            expectCount(name, 1, args.size());
            long long n = asInteger(name, args[0]);
            if (n < 0) {
                throw std::runtime_error("ones expects non-negative length");
            }
            return Value(std::vector<std::complex<double>>(
                static_cast<std::size_t>(n), std::complex<double>(1.0, 0.0)));
        }
        if (name == "dot" || name == "scalar") {
            expectCount(name, 2, args.size());
            const auto& a = requireVector(name, args[0]);
            const auto& b = requireVector(name, args[1]);
            if (a.size() != b.size()) {
                throw std::runtime_error(name +
                                         " expects vectors of equal length");
            }
            std::complex<double> sum(0.0, 0.0);
            for (std::size_t i = 0; i < a.size(); ++i) {
                sum += a[i] * b[i];
            }
            return Value(sum);
        }
        if (name == "phase") {
            expectCount(name, 1, args.size());
            return Value(std::arg(requireScalar(name, args[0])));
        }
        if (name == "polar") {
            expectCount(name, 1, args.size());
            double r = std::abs(requireScalar(name, args[0]));
            double phi = std::arg(requireScalar(name, args[0]));
            return Value(std::complex<double>(r, phi));
        }
        if (name == "rect") {
            expectCount(name, 2, args.size());
            double r = requireReal(name, args[0]);
            double phi = requireReal(name, args[1]);
            return Value(std::polar(r, phi));
        }
        if (name == "complex") {
            expectCount(name, 2, args.size());
            return Value(std::complex<double>(requireReal(name, args[0]),
                                              requireReal(name, args[1])));
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
        if (check(TokenType::LBracket)) {
            advance();
            std::vector<std::complex<double>> elements;
            if (!check(TokenType::RBracket)) {
                while (true) {
                    Value element = expression();
                    if (!element.isScalar()) {
                        throw std::runtime_error(
                            "Vectors cannot contain other vectors");
                    }
                    elements.push_back(element.data);
                    if (check(TokenType::Comma)) {
                        advance();
                        continue;
                    }
                    break;
                }
            }
            consume(TokenType::RBracket, "Expected ']'.");
            return Value(std::move(elements));
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

std::unordered_map<std::string, Value> makeDefaultEnv() {
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
    return env;
}

void repl() {
    std::unordered_map<std::string, Value> env = makeDefaultEnv();
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
        if (line == "help") {
            std::cout << kHelpText;
            continue;
        }
        if (line.empty()) {
            continue;
        }
        try {
            Lexer lexer(line);
            auto tokens = lexer.tokenize();
            Parser parser(std::move(tokens), env);
            ParseResult result = parser.parse();
            env["ans"] = result.value;
            if (!result.suppressOutput) {
                printValue(result.value);
            }
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
