#ifndef FLOAT34_H
#define FLOAT34_H

#include <cstdint>
#include <iostream>
#include <cmath>

struct float34 {
    uint64_t data;

    static const uint64_t M_MASK = 0xFFFFFFULL;
    static const uint64_t E_MASK = 0x1FFULL;
    static const uint64_t INF_E = 0x1FFULL;
    static const int BIAS = 255;
    static const uint64_t HIDDEN = 1ULL << 24;

    float34() : data(0) {}
    float34(uint64_t s, uint64_t e, uint64_t m) {
        data = ((s & 1ULL) << 33) | ((e & E_MASK) << 24) | (m & M_MASK);
    }

    uint64_t sign() const { return (data >> 33) & 1ULL; }
    uint64_t exp()  const { return (data >> 24) & E_MASK; }
    uint64_t mant() const { return data & M_MASK; }

    // ---------- SPECIAL ----------
    static float34 Zero() { return float34(0, 0, 0); }
    static float34 NegZero() { return float34(1, 0, 0); }
    static float34 Inf() { return float34(0, INF_E, 0); }
    static float34 NInf() { return float34(1, INF_E, 0); }
    static float34 NaN() { return float34(0, INF_E, 1); }

    bool isNaN()  const { return exp() == INF_E && mant() != 0; }
    bool isInf()  const { return exp() == INF_E && mant() == 0; }
    bool isZero() const { return exp() == 0 && mant() == 0; }

    // ---------- ROUND ----------
    static uint64_t roundGRS(uint64_t v, int shift) {
        if (shift <= 0) return v;

        uint64_t guard = (v >> (shift - 1)) & 1;
        uint64_t round = (shift > 1) ? ((v >> (shift - 2)) & 1) : 0;
        uint64_t sticky = (shift > 1) ? (v & ((1ULL << (shift - 2)) - 1)) : 0;

        uint64_t res = v >> shift;

        if (guard && (round || sticky || (res & 1)))
            res++;

        return res;
    }

    // ---------- NORMALIZE ----------
    static void normalize(uint64_t& m, int64_t& e) {
        while (m && m < HIDDEN) {
            m <<= 1;
            e--;
        }
        while (m >= (HIDDEN << 1)) {
            m >>= 1;
            e++;
        }
    }

    // ---------- TO DOUBLE ----------
    double toDouble() const {
        if (isZero()) return sign() ? -0.0 : 0.0;
        if (isInf())  return sign() ? -INFINITY : INFINITY;
        if (isNaN())  return NAN;

        double m = mant() / (double)HIDDEN;

        double val;
        if (exp() == 0)
            val = m * std::pow(2.0, 1 - BIAS);
        else
            val = (1.0 + m) * std::pow(2.0, (int)exp() - BIAS);

        return sign() ? -val : val;
    }

    // ---------- FROM DOUBLE ----------
    static float34 fromDouble(double v) {
        if (std::isnan(v)) return NaN();
        if (std::isinf(v)) return v < 0 ? NInf() : Inf();
        if (v == 0.0) return std::signbit(v) ? NegZero() : Zero();

        uint64_t s = std::signbit(v);
        v = std::fabs(v);

        int e;
        double m = std::frexp(v, &e);

        int64_t exp = e + BIAS - 1;

        if (exp >= INF_E) return s ? NInf() : Inf();

        double frac = m * 2.0 - 1.0;
        uint64_t raw = (uint64_t)(frac * (1ULL << 26));

        uint64_t mant = roundGRS(raw, 2);

        if (mant >= HIDDEN) {
            mant = 0;
            exp++;
        }

        if (exp <= 0) {
            int shift = 1 - exp;
            if (shift < 64)
                mant = roundGRS(mant, shift);
            else
                mant = 0;
            exp = 0;
        }

        return float34(s, exp, mant);
    }

    // ---------- ADD ----------
    float34 operator+(const float34& b) const {
        if (isNaN() || b.isNaN()) return NaN();

        if (isInf() || b.isInf()) {
            if (isInf() && b.isInf() && sign() != b.sign())
                return NaN();
            return isInf() ? *this : b;
        }

        uint64_t e1 = exp(), e2 = b.exp();

        uint64_t m1 = mant();
        uint64_t m2 = b.mant();

        if (e1) m1 |= HIDDEN;
        if (e2) m2 |= HIDDEN;

        int64_t diff = (int64_t)e1 - (int64_t)e2;

        if (diff > 0) m2 >>= diff;
        else if (diff < 0) m1 >>= -diff;

        int64_t e = std::max(e1, e2);

        int64_t m;
        uint64_t s;

        if (sign() == b.sign()) {
            m = m1 + m2;
            s = sign();
        }
        else {
            if (m1 >= m2) { m = m1 - m2; s = sign(); }
            else { m = m2 - m1; s = b.sign(); }
        }

        if (m == 0) return Zero();

        normalize((uint64_t&)m, e);

        uint64_t r = roundGRS(m, 0);

        if (e >= INF_E) return float34(s, INF_E, 0);
        if (e <= 0) return Zero();

        return float34(s, e, r & M_MASK);
    }

    float34 operator-(const float34& b) const {
        float34 t = b;
        t.data ^= (1ULL << 33);
        return *this + t;
    }

    // ---------- MUL ----------
    float34 operator*(const float34& b) const {
        if (isNaN() || b.isNaN()) return NaN();

        uint64_t s = sign() ^ b.sign();

        if (isInf() || b.isInf()) return float34(s, INF_E, 0);

        uint64_t a = mant() | HIDDEN;
        uint64_t c = b.mant() | HIDDEN;

        uint64_t m = (a * c) >> 24;

        int64_t e = exp() + b.exp() - BIAS;

        normalize(m, e);

        uint64_t r = roundGRS(m, 0);

        if (e >= INF_E) return float34(s, INF_E, 0);
        if (e <= 0) return Zero();

        return float34(s, e, r & M_MASK);
    }

    // ---------- DIV ----------
    float34 operator/(const float34& b) const {
        if (isNaN() || b.isNaN()) return NaN();
        if (b.isZero()) return float34(sign() ^ b.sign(), INF_E, 0);

        uint64_t s = sign() ^ b.sign();

        uint64_t a = mant() | HIDDEN;
        uint64_t c = b.mant() | HIDDEN;

        uint64_t res = 0;
        uint64_t rem = a;

        for (int i = 0; i < 27; i++) {
            res <<= 1;
            if (rem >= c) {
                rem -= c;
                res |= 1;
            }
            rem <<= 1;
        }

        int64_t e = exp() - b.exp() + BIAS;

        uint64_t r = roundGRS(res, 0);

        if (e >= INF_E) return float34(s, INF_E, 0);
        if (e <= 0) return Zero();

        return float34(s, e, r & M_MASK);
    }

    static void print(float34 v) {
        if (v.isNaN()) { std::cout << "nan"; return; }
        if (v.isInf()) { std::cout << (v.sign() ? "-" : "") << "inf"; return; }
        std::cout << v.toDouble();
    }
};

#endif