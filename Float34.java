package net.minecraft.world.entity.monster;

public final class Float34 {

    // ================= FORMAT =================
    // 1 bit sign | 9 bit exponent | 24 bit mantissa

    private long data;

    private static final long M_MASK = 0xFFFFFFL;
    private static final long E_MASK = 0x1FFL;
    private static final long INF_E = 0x1FFL;
    private static final int BIAS = 255;
    private static final long HIDDEN = 1L << 24;

    // ================= SPECIAL CONSTANTS =================
    public static final Float34 POSITIVE_INFINITY = new Float34(0, INF_E, 0);
    public static final Float34 NEGATIVE_INFINITY = new Float34(1, INF_E, 0);
    public static final Float34 NaN = new Float34(0, INF_E, 1);

    public static final Float34 MAX_VALUE = new Float34(0, INF_E - 1, M_MASK);
    public static final Float34 MIN_VALUE = new Float34(0, 1, 1);
    public static final Float34 ZERO = new Float34(0, 0, 0);
    private long value;

    // ================= CONSTRUCTORS =================
    public Float34() {
        this.data = 0;
    }

    public Float34(long bits) {
    }

    public boolean eq(Float34 other) {
        return this.sign() == other.sign()
                && this.exp() == other.exp()
                && this.mant() == other.mant();
    }

    public static Float34 fromDouble(double d) {
        long bits = Double.doubleToRawLongBits(d);
        return new Float34(bits);
    }

 
    
    public double toDouble() {
        // обратное преобразование
        return Double.longBitsToDouble(this.value);
    }

    public Float34(long s, long e, long m) {
        this.data =
                ((s & 1L) << 33) |
                        ((e & E_MASK) << 24) |
                        (m & M_MASK);
    }

    // ================= ACCESS =================
    public long sign() { return data >>> 33; }
    public long exp()  { return (data >>> 24) & E_MASK; }
    public long mant() { return data & M_MASK; }

    // ================= STATE =================
    public boolean isNaN() { return exp() == INF_E && mant() != 0; }
    public boolean isInf() { return exp() == INF_E && mant() == 0; }
    public boolean isZero() { return exp() == 0 && mant() == 0; }

    // ================= FACTORY =================
    public static Float34 fromLong(long v) {
        if (v == 0) return ZERO;

        long sign = v < 0 ? 1 : 0;
        long x = Math.abs(v);

        int msb = 63 - Long.numberOfLeadingZeros(x);
        long e = msb + BIAS;

        long m = x << (24 - msb);
        return new Float34(sign, e, m & M_MASK);
    }

    public long toLong() {
        if (isZero()) return 0;
        if (isInf() || isNaN()) throw new ArithmeticException("Invalid Float34");

        long e = exp();
        long m = mant();

        m |= HIDDEN;

        int shift = (int)(e - BIAS - 24);

        if (shift >= 0) return (sign() == 1 ? -1 : 1) * (m << shift);
        else return (sign() == 1 ? -1 : 1) * (m >> -shift);
    }

    // ================= ADD =================
    public Float34 add(Float34 b) {
        if (isNaN() || b.isNaN()) return NaN;

        long ma = mant() | (exp() != 0 ? HIDDEN : 0);
        long mb = b.mant() | (b.exp() != 0 ? HIDDEN : 0);

        long ea = exp();
        long eb = b.exp();

        if (ea > eb) mb >>= (ea - eb);
        else ma >>= (eb - ea);

        long er = Math.max(ea, eb);

        long mr;
        long sr;

        if (sign() == b.sign()) {
            mr = ma + mb;
            sr = sign();
        } else {
            if (ma >= mb) {
                mr = ma - mb;
                sr = sign();
            } else {
                mr = mb - ma;
                sr = b.sign();
            }
        }

        if (mr == 0) return ZERO;

        while (mr >= (HIDDEN << 1)) {
            mr >>= 1;
            er++;
        }
        while (mr < HIDDEN && er > 0) {
            mr <<= 1;
            er--;
        }

        if (er >= INF_E) return POSITIVE_INFINITY;
        if (er <= 0) return ZERO;

        return new Float34(sr, er, mr & M_MASK);
    }

    // ================= SUB =================
    public Float34 sub(Float34 b) {
        return add(new Float34(b.sign() ^ 1, b.exp(), b.mant()));
    }

    // ================= MUL =================
    public Float34 mul(Float34 b) {
        if (isNaN() || b.isNaN()) return NaN;

        if (isZero() || b.isZero()) return ZERO;

        if (isInf() || b.isInf())
            return new Float34(sign() ^ b.sign(), INF_E, 0);

        long ma = mant() | HIDDEN;
        long mb = b.mant() | HIDDEN;

        long sr = sign() ^ b.sign();
        long er = exp() + b.exp() - BIAS;

        long mr = ma * mb;

        if ((mr & (1L << 49)) != 0) {
            mr >>= 25;
            er++;
        } else {
            mr >>= 24;
        }

        if (er >= INF_E) return POSITIVE_INFINITY;
        if (er <= 0) return ZERO;

        return new Float34(sr, er, mr & M_MASK);
    }

    // ================= DIV =================
    public Float34 div(Float34 b) {
        if (isNaN() || b.isNaN()) return NaN;

        if (b.isZero()) return POSITIVE_INFINITY;
        if (isZero()) return ZERO;

        long ma = mant() | HIDDEN;
        long mb = b.mant() | HIDDEN;

        long er = exp() - b.exp() + BIAS;

        long mr = (ma << 24) / mb;

        while (mr < HIDDEN) {
            mr <<= 1;
            er--;
        }

        if (er >= INF_E) return POSITIVE_INFINITY;
        if (er <= 0) return ZERO;

        return new Float34(sign() ^ b.sign(), er, mr & M_MASK);
    }

    // ================= SQRT =================
    public Float34 sqrt() {
        if (isNaN()) return NaN;
        if (sign() == 1) return NaN;
        if (isZero() || isInf()) return this;

        long e = exp();
        long m = mant() | HIDDEN;

        long realE = e - BIAS;

        if ((realE & 1) != 0) {
            realE--;
            m <<= 1;
        }

        long resE = (realE / 2) + BIAS;

        long x = m << 24;
        long g = HIDDEN;

        for (int i = 0; i < 8; i++) {
            g = (g + x / g) >> 1;
        }

        if (g >= (HIDDEN << 1)) {
            g >>= 1;
            resE++;
        }

        return new Float34(0, resE, g & M_MASK);
    }

    // ================= STRING =================
    @Override
    public String toString() {
        if (isNaN()) return "NaN";
        if (isInf()) return sign() == 1 ? "-Infinity" : "Infinity";
        return Long.toString(toLong());
    }
}