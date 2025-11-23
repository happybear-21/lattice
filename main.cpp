
#include <cctype>
#include <cmath>
#include <complex>
#include <functional>
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

const char* kHelpText =
    "Lattice quick help:\n"
    "  - Scalars: default numbers are real; complex results print as (a,b).\n"
    "  - Complex constructor: complex(real_part, imag_part)\n"
    "  - Arrays (1-D literals only): [1, 2, 3]; elementwise + -; * allowed "
    "only scalar*array.\n"
    "    Array helpers: zeros(shape), ones(shape), empty(shape), full(shape, "
    "v), copy(v), dot(a, b) (alias scalar, 1-D), *_like variants, eye/identity "
    "(2-D). Shapes can be scalar (n) or 1-D shape arrays like [2,3].\n"
    "  - DTypes/casting: bool, int/int64, int32, int16, int8, uint{8,16,32,64}, "
    "float/float64, float32, float16, complex64, complex128.\n"
    "  - Assignment: x = 5, v = [1,2]. Last value in ans.\n"
    "  - Print: print expr\n"
    "  - Input: input name   (prompts on the next line)\n"
    "  - Constants: pi, tau, e, inf, nan, infj, nanj\n"
    "  - Functions: trig (sin/cos/tan...), exp/log variants, factorial, gcd, "
    "lcm, comb, perm, dist, hypot, fsum, prod, sumprod, phase, polar, rect, "
    "and more. See source for full list.\n"
    "Type quit or exit to leave.\n";

enum class DType {
    Bool,
    Int8,
    UInt8,
    Int16,
    UInt16,
    Int32,
    UInt32,
    Int64,
    UInt64,
    Float16,
    Float32,
    Float64,
    Complex64,
    Complex128
};

const std::unordered_set<std::string> kReservedNames = {
    "pi",         "tau",      "e",        "inf",       "nan",
    "infj",       "nanj",     "print",    "input",     "bool",
    "int",        "int64",    "int32",    "int16",     "int8",
    "uint64",     "uint32",   "uint16",   "uint8",     "float",
    "float64",    "float32",  "float16",  "complex64", "complex128"};

std::optional<DType> dtypeFromName(const std::string& name) {
    if (name == "bool") {
        return DType::Bool;
    }
    if (name == "int" || name == "int64") {
        return DType::Int64;
    }
    if (name == "int32") {
        return DType::Int32;
    }
    if (name == "int16") {
        return DType::Int16;
    }
    if (name == "int8") {
        return DType::Int8;
    }
    if (name == "uint64") {
        return DType::UInt64;
    }
    if (name == "uint32") {
        return DType::UInt32;
    }
    if (name == "uint16") {
        return DType::UInt16;
    }
    if (name == "uint8") {
        return DType::UInt8;
    }
    if (name == "float" || name == "float64") {
        return DType::Float64;
    }
    if (name == "float32") {
        return DType::Float32;
    }
    if (name == "float16") {
        return DType::Float16;
    }
    if (name == "complex64") {
        return DType::Complex64;
    }
    if (name == "complex128") {
        return DType::Complex128;
    }
    return std::nullopt;
}

struct Value {
    enum class Kind { Scalar, Array };

    Kind kind = Kind::Scalar;
    DType dtype = DType::Float64;
    bool isString = false;
    std::string str;
    std::complex<double> scalar{0.0, 0.0};
    std::vector<std::complex<double>> array;
    std::vector<std::size_t> shape;

    Value()
        : dtype(DType::Float64) {}
    explicit Value(double real, DType dt = DType::Float64)
        : kind(Kind::Scalar)
        , dtype(dt)
        , scalar(real, 0.0) {}
    explicit Value(std::complex<double> value,
                   DType dt = DType::Complex128)
        : kind(Kind::Scalar)
        , dtype(dt)
        , scalar(value) {}
    Value(std::vector<std::complex<double>> values,
          std::vector<std::size_t> dims,
          DType dt = DType::Float64)
        : kind(Kind::Array)
        , dtype(dt)
        , array(std::move(values))
        , shape(std::move(dims)) {}

    static Value makeString(std::string text) {
        Value v;
        v.isString = true;
        v.str = std::move(text);
        return v;
    }

    bool isScalar() const {
        return kind == Kind::Scalar;
    }

    bool isArray() const {
        return kind == Kind::Array;
    }

    double real() const {
        if (!isScalar()) {
            throw std::runtime_error("Expected scalar value");
        }
        return scalar.real();
    }

    double imag() const {
        if (!isScalar()) {
            throw std::runtime_error("Expected scalar value");
        }
        return scalar.imag();
    }

    std::size_t size() const {
        return array.size();
    }

    bool isZero() const {
        if (!isScalar()) {
            throw std::runtime_error("Expected scalar value");
        }
        return scalar.real() == 0.0 && scalar.imag() == 0.0;
    }
};

int dtypeRank(DType dt) {
    switch (dt) {
    case DType::Bool:
        return 0;
    case DType::Int8:
        return 1;
    case DType::UInt8:
        return 2;
    case DType::Int16:
        return 3;
    case DType::UInt16:
        return 4;
    case DType::Int32:
        return 5;
    case DType::UInt32:
        return 6;
    case DType::Int64:
        return 7;
    case DType::UInt64:
        return 8;
    case DType::Float16:
        return 9;
    case DType::Float32:
        return 10;
    case DType::Float64:
        return 11;
    case DType::Complex64:
        return 12;
    case DType::Complex128:
        return 13;
    }
    return 0;
}

DType promote(DType a, DType b) {
    return dtypeRank(a) >= dtypeRank(b) ? a : b;
}

std::string dtypeName(DType dt) {
    switch (dt) {
    case DType::Bool:
        return "bool";
    case DType::Int8:
        return "int8";
    case DType::UInt8:
        return "uint8";
    case DType::Int16:
        return "int16";
    case DType::UInt16:
        return "uint16";
    case DType::Int32:
        return "int32";
    case DType::UInt32:
        return "uint32";
    case DType::Int64:
        return "int64";
    case DType::UInt64:
        return "uint64";
    case DType::Float16:
        return "float16";
    case DType::Float32:
        return "float32";
    case DType::Float64:
        return "float64";
    case DType::Complex64:
        return "complex64";
    case DType::Complex128:
        return "complex128";
    }
    return "unknown";
}

std::complex<double> castScalar(const std::complex<double>& value,
                                DType target) {
    double re = value.real();
    double im = value.imag();
    auto requireReal = [&](const char* name) {
        if (im != 0.0) {
            throw std::runtime_error(std::string(name) +
                                     " expects real argument");
        }
    };
    switch (target) {
    case DType::Bool:
        return std::complex<double>((re != 0.0 || im != 0.0) ? 1.0 : 0.0, 0.0);
    case DType::Int8:
    case DType::Int16:
    case DType::Int32:
    case DType::Int64:
        requireReal("int");
        return std::complex<double>(std::llround(re), 0.0);
    case DType::UInt8:
    case DType::UInt16:
    case DType::UInt32:
    case DType::UInt64: {
        requireReal("uint");
        long long rounded = std::llround(re);
        if (rounded < 0) {
            throw std::runtime_error("uint expects non-negative value");
        }
        return std::complex<double>(static_cast<double>(rounded), 0.0);
    }
    case DType::Float16:
    case DType::Float32:
    case DType::Float64:
        requireReal("float");
        return std::complex<double>(re, 0.0);
    case DType::Complex64:
    case DType::Complex128:
        return value;
    }
    return value;
}

Value castValue(const Value& v, DType dt) {
    if (v.isScalar()) {
        return Value(castScalar(v.scalar, dt), dt);
    }
    std::vector<std::complex<double>> out;
    out.reserve(v.array.size());
    for (const auto& c : v.array) {
        out.push_back(castScalar(c, dt));
    }
    return Value(std::move(out), v.shape, dt);
}

std::string complexToString(const std::complex<double>& c, DType dt) {
    if (dt == DType::Bool) {
        return (c.real() != 0.0 || c.imag() != 0.0) ? "true" : "false";
    }
    bool isIntType = dt == DType::Int8 || dt == DType::Int16 ||
                     dt == DType::Int32 || dt == DType::Int64 ||
                     dt == DType::UInt8 || dt == DType::UInt16 ||
                     dt == DType::UInt32 || dt == DType::UInt64;
    bool isFloatType = dt == DType::Float16 || dt == DType::Float32 ||
                       dt == DType::Float64 || dt == DType::Complex64 ||
                       dt == DType::Complex128;
    std::ostringstream oss;
    if (c.imag() == 0.0) {
        if (isIntType) {
            oss << std::llround(c.real());
        } else if (isFloatType) {
            oss.setf(std::ios::showpoint);
            oss << c.real();
        } else {
            oss << c.real();
        }
        return oss.str();
    }
    oss << c;
    return oss.str();
}

std::string toString(const Value& v) {
    if (v.isString) {
        return v.str;
    }
    if (v.isScalar()) {
        return complexToString(v.scalar, v.dtype);
    }
    std::ostringstream oss;
    if (v.shape.empty()) {
        return "[]";
    }

    std::function<void(std::size_t, std::size_t, std::size_t)> printDim;
    printDim = [&](std::size_t dim, std::size_t offset, std::size_t stride) {
        oss << '[';
        if (dim + 1 == v.shape.size()) {
            for (std::size_t i = 0; i < v.shape[dim]; ++i) {
                if (i != 0) {
                    oss << ", ";
                }
                oss << complexToString(v.array[offset + i * stride], v.dtype);
            }
        } else {
            std::size_t nextStride = stride / v.shape[dim + 1];
            for (std::size_t i = 0; i < v.shape[dim]; ++i) {
                if (i != 0) {
                    oss << ", ";
                }
                printDim(dim + 1, offset + i * stride, nextStride);
            }
        }
        oss << ']';
    };

    std::size_t stride = 1;
    for (std::size_t d = 1; d < v.shape.size(); ++d) {
        stride *= v.shape[d];
    }
    printDim(0, 0, stride);
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
    DType resultType = promote(a.dtype, b.dtype);
    if (a.isString || b.isString) {
        throw std::runtime_error(name + " expects numeric arguments");
    }
    if (a.isScalar() && b.isScalar()) {
        return Value(op(a.scalar, b.scalar), resultType);
    }
    if (a.isArray() && b.isScalar()) {
        std::vector<std::complex<double>> result = a.array;
        for (auto& val : result) {
            val = op(val, b.scalar);
        }
        return Value(std::move(result), a.shape, resultType);
    }
    if (a.isScalar() && b.isArray()) {
        std::vector<std::complex<double>> result = b.array;
        for (auto& val : result) {
            val = op(a.scalar, val);
        }
        return Value(std::move(result), b.shape, resultType);
    }
    if (a.isArray() && b.isArray()) {
        if (a.shape != b.shape) {
            throw std::runtime_error(name + " expects arrays of same shape");
        }
        std::vector<std::complex<double>> result;
        result.reserve(a.array.size());
        for (std::size_t i = 0; i < a.array.size(); ++i) {
            result.push_back(op(a.array[i], b.array[i]));
        }
        return Value(std::move(result), a.shape, resultType);
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
    if (a.isArray() && b.isArray()) {
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
    if (b.isArray()) {
        for (const auto& val : b.array) {
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
        return Value(-v.scalar, v.dtype);
    }
    std::vector<std::complex<double>> result = v.array;
    for (auto& entry : result) {
        entry = -entry;
    }
    return Value(std::move(result), v.shape, v.dtype);
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
        if (v.isString) {
            throw std::runtime_error(name + " expects numeric arguments");
        }
        if (!v.isScalar()) {
            throw std::runtime_error(name + " expects scalar arguments");
        }
        return v.scalar;
    }

    const std::vector<std::complex<double>>&
    requireVector(const std::string& name, const Value& v) const {
        if (v.isString) {
            throw std::runtime_error(name + " expects array arguments");
        }
        if (!v.isArray()) {
            throw std::runtime_error(name + " expects array arguments");
        }
        return v.array;
    }

    const std::vector<std::size_t>& shapeOf(const std::string& name,
                                            const Value& v) const {
        if (!v.isArray()) {
            throw std::runtime_error(name + " expects array arguments");
        }
        return v.shape;
    }

    std::size_t asSize(const std::string& name, const Value& v) const {
        if (v.isString) {
            throw std::runtime_error(name + " expects numeric dimensions");
        }
        long long n = asInteger(name, v);
        if (n < 0) {
            throw std::runtime_error(name + " expects non-negative length");
        }
        return static_cast<std::size_t>(n);
    }

    Value makeFilled(const std::vector<std::size_t>& dims,
                     std::complex<double> fill,
                     DType dt = DType::Float64) {
        std::size_t total = 1;
        for (auto d : dims) {
            total *= d;
        }
        return Value(std::vector<std::complex<double>>(total, fill), dims, dt);
    }

    Value makeEmpty(const std::vector<std::size_t>& dims,
                    DType dt = DType::Float64) {
        std::size_t total = 1;
        for (auto d : dims) {
            total *= d;
        }
        return Value(
            std::vector<std::complex<double>>(total,
                                              std::numeric_limits<double>::quiet_NaN()),
            dims, dt);
    }

    std::vector<std::size_t> shapeFromValue(const std::string& name,
                                            const Value& v) const {
        if (v.isString) {
            throw std::runtime_error(name + " expects numeric dimensions");
        }
        if (v.isScalar()) {
            return {asSize(name, v)};
        }
        // When shape is provided as an array value, interpret elements as dims.
        if (v.shape.size() == 1) {
            std::vector<std::size_t> dims;
            dims.reserve(v.array.size());
            for (const auto& c : v.array) {
                double re = c.real();
                if (!std::isfinite(re) || std::floor(re) != re) {
                    throw std::runtime_error(name +
                                             " expects integer dimensions");
                }
                if (re < 0) {
                    throw std::runtime_error(
                        name + " expects non-negative dimensions");
                }
                dims.push_back(static_cast<std::size_t>(re));
            }
            return dims;
        }
        return v.shape;
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
        auto castHelper = [&](const std::string& fname, DType target) {
            expectCount(fname, 1, args.size());
            return castValue(args[0], target);
        };
        auto dtypeOf = [&](const Value& v) -> DType { return v.dtype; };
        if (name == "bool") {
            return castHelper(name, DType::Bool);
        }
        if (name == "int" || name == "int64") {
            return castHelper(name, DType::Int64);
        }
        if (name == "int32") {
            return castHelper(name, DType::Int32);
        }
        if (name == "int16") {
            return castHelper(name, DType::Int16);
        }
        if (name == "int8") {
            return castHelper(name, DType::Int8);
        }
        if (name == "uint64") {
            return castHelper(name, DType::UInt64);
        }
        if (name == "uint32") {
            return castHelper(name, DType::UInt32);
        }
        if (name == "uint16") {
            return castHelper(name, DType::UInt16);
        }
        if (name == "uint8") {
            return castHelper(name, DType::UInt8);
        }
        if (name == "float" || name == "float64") {
            return castHelper(name, DType::Float64);
        }
        if (name == "float32") {
            return castHelper(name, DType::Float32);
        }
        if (name == "float16") {
            return castHelper(name, DType::Float16);
        }
        if (name == "complex64") {
            return castHelper(name, DType::Complex64);
        }
        if (name == "complex128") {
            return castHelper(name, DType::Complex128);
        }
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
            if (!args[0].isArray()) {
                throw std::runtime_error("copy expects array arguments");
            }
            return Value(args[0].array, args[0].shape, args[0].dtype);
        }
        if (name == "type") {
            expectCount(name, 1, args.size());
            return Value::makeString(dtypeName(args[0].dtype));
        }
        if (name == "zeros") {
            if (args.size() < 1 || args.size() > 2) {
                throw std::runtime_error("zeros expects 1 or 2 arguments");
            }
            DType dt = args.size() == 2 ? dtypeOf(args[1]) : DType::Float64;
            return makeFilled(shapeFromValue(name, args[0]),
                              std::complex<double>(0.0, 0.0), dt);
        }
        if (name == "ones") {
            if (args.size() < 1 || args.size() > 2) {
                throw std::runtime_error("ones expects 1 or 2 arguments");
            }
            DType dt = args.size() == 2 ? dtypeOf(args[1]) : DType::Float64;
            return makeFilled(shapeFromValue(name, args[0]),
                              std::complex<double>(1.0, 0.0), dt);
        }
        if (name == "empty") {
            if (args.size() < 1 || args.size() > 2) {
                throw std::runtime_error("empty expects 1 or 2 arguments");
            }
            DType dt = args.size() == 2 ? dtypeOf(args[1]) : DType::Float64;
            return makeEmpty(shapeFromValue(name, args[0]), dt);
        }
        if (name == "full") {
            if (args.size() < 2 || args.size() > 3) {
                throw std::runtime_error("full expects 2 or 3 arguments");
            }
            DType dt =
                args.size() == 3 ? dtypeOf(args[2]) : args[1].dtype;
            return makeFilled(shapeFromValue(name, args[0]),
                              requireScalar(name, args[1]), dt);
        }
        if (name == "dot" || name == "scalar") {
            expectCount(name, 2, args.size());
            const auto& a = requireVector(name, args[0]);
            const auto& b = requireVector(name, args[1]);
            if (args[0].shape.size() != 1 || args[1].shape.size() != 1) {
                throw std::runtime_error(name + " expects 1-D arrays");
            }
            if (a.size() != b.size()) {
                throw std::runtime_error(name +
                                         " expects arrays of equal length");
            }
            std::complex<double> sum(0.0, 0.0);
            for (std::size_t i = 0; i < a.size(); ++i) {
                sum += a[i] * b[i];
            }
            return Value(sum, promote(args[0].dtype, args[1].dtype));
        }
        if (name == "empty_like") {
            expectCount(name, 1, args.size());
            return makeEmpty(shapeOf(name, args[0]), args[0].dtype);
        }
        if (name == "zeros_like") {
            expectCount(name, 1, args.size());
            return makeFilled(shapeOf(name, args[0]),
                              std::complex<double>(0.0, 0.0), args[0].dtype);
        }
        if (name == "ones_like") {
            expectCount(name, 1, args.size());
            return makeFilled(shapeOf(name, args[0]),
                              std::complex<double>(1.0, 0.0), args[0].dtype);
        }
        if (name == "full_like") {
            expectCount(name, 2, args.size());
            return makeFilled(shapeOf(name, args[0]),
                              requireScalar(name, args[1]), args[1].dtype);
        }
        if (name == "eye") {
            if (args.size() < 1 || args.size() > 3) {
                throw std::runtime_error("eye expects 1, 2, or 3 arguments");
            }
            std::size_t n = asSize(name, args[0]);
            std::size_t m = args.size() >= 2 ? asSize(name, args[1]) : n;
            long long k = args.size() == 3 ? asInteger(name, args[2]) : 0;
            std::vector<std::complex<double>> mat(
                n * m, std::complex<double>(0.0, 0.0));
            for (std::size_t row = 0; row < n; ++row) {
                long long col = static_cast<long long>(row) + k;
                if (col >= 0 && static_cast<std::size_t>(col) < m) {
                    mat[row * m + static_cast<std::size_t>(col)] =
                        std::complex<double>(1.0, 0.0);
                }
            }
            return Value(std::move(mat), {n, m}, DType::Float64);
        }
        if (name == "identity") {
            expectCount(name, 1, args.size());
            std::size_t n = asSize(name, args[0]);
            std::vector<std::complex<double>> mat(
                n * n, std::complex<double>(0.0, 0.0));
            for (std::size_t i = 0; i < n; ++i) {
                mat[i * n + i] = std::complex<double>(1.0, 0.0);
            }
            return Value(std::move(mat), {n, n}, DType::Float64);
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
            DType litType = DType::Float64;
            if (!check(TokenType::RBracket)) {
                while (true) {
                    Value element = expression();
                    if (!element.isScalar()) {
                        throw std::runtime_error(
                            "Vectors cannot contain other vectors");
                    }
                    litType = promote(litType, element.dtype);
                    elements.push_back(element.scalar);
                    if (check(TokenType::Comma)) {
                        advance();
                        continue;
                    }
                    break;
                }
            }
            consume(TokenType::RBracket, "Expected ']'.");
            return Value(std::move(elements), {elements.size()}, litType);
        }
        if (check(TokenType::Identifier)) {
            auto name = advance().lexeme;
            if (auto maybeDt = dtypeFromName(name)) {
                return Value(0.0, *maybeDt);
            }
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
