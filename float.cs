using System;

public struct Float34
{
    // [0]-Знак(1)+Эксп(7), [1]-Эксп(2)+Мантисса(6), [2,3,4]-Мантисса(24)
    public byte[] Data;

    public Float34(bool init = true) { Data = new byte[5]; }

    // --- Свойства IEEE ---
    public bool IsNeg => (Data[0] & 0x80) != 0;
    public int GetExp() => ((Data[0] & 0x7F) << 2) | (Data[1] >> 6);
    private bool IsMantZero() => (Data[1] & 0x3F) == 0 && Data[2] == 0 && Data[3] == 0 && Data[4] == 0;
    public bool IsNaN => GetExp() == 0x1FF && !IsMantZero();
    public bool IsInf => GetExp() == 0x1FF && IsMantZero();
    public bool IsZero => GetExp() == 0 && IsMantZero();

    // --- Математика ---

    public static Float34 operator +(Float34 a, Float34 b)
    {
        if (a.IsNaN || b.IsNaN) return CreateNaN();
        if (a.IsInf) return (b.IsInf && a.IsNeg != b.IsNeg) ? CreateNaN() : a;
        if (b.IsInf) return b;

        if (a.IsNeg == b.IsNeg)
        {
            return AddOrSub(a, b, false); // Сложение
        }
        else
        {
            return AddOrSub(a, b, true);  // Вычитание (разные знаки)
        }
    }

    public static Float34 operator -(Float34 a, Float34 b)
    {
        // Вычитание b - это сложение с инвертированным знаком b
        Float34 negB = b;
        negB.Data[0] ^= 0x80;
        return a + negB;
    }

    private static Float34 AddOrSub(Float34 a, Float34 b, bool subtract)
    {
        int expA = a.GetExp();
        int expB = b.GetExp();
        byte[] mA = GetMantissaArray(a);
        byte[] mB = GetMantissaArray(b);

        // Выравнивание экспонент
        int resExp = Math.Max(expA, expB);
        if (expA > expB) ShiftRight(mB, expA - expB);
        else if (expB > expA) ShiftRight(mA, expB - expA);

        byte[] resM = new byte[4];
        bool resSign = a.IsNeg;

        if (subtract)
        {
            if (Compare(mA, mB) >= 0) Subtract(mA, mB, resM);
            else { Subtract(mB, mA, resM); resSign = !resSign; }
        }
        else
        {
            if (Add(mA, mB, resM)) { ShiftRight(resM, 1); SetBit(resM, 30); resExp++; }
        }

        // Нормализация результата
        while (resExp > 0 && !GetBit(resM, 30)) { ShiftLeft(resM); resExp--; }

        return Pack(resSign, resExp, resM);
    }

    public static Float34 operator *(Float34 a, Float34 b)
    {
        if (a.IsNaN || b.IsNaN) return CreateNaN();
        if (a.IsInf || b.IsInf) return CreateInf(a.IsNeg ^ b.IsNeg);
        if (a.IsZero || b.IsZero) return new Float34(true);

        int resExp = a.GetExp() + b.GetExp() - 255;
        byte[] mA = GetMantissaArray(a);
        byte[] mB = GetMantissaArray(b);
        byte[] mRes = new byte[8]; // Двойной буфер для умножения

        for (int i = 0; i < 31; i++) if (GetBit(mB, i)) AddToLarge(mRes, mA, i);

        byte[] finalM = new byte[4];
        Array.Copy(mRes, 0, finalM, 0, 4);
        if (GetBit(finalM, 31)) { ShiftRight(finalM, 1); resExp++; }
        else { ShiftLeft(finalM); }

        return Pack(a.IsNeg ^ b.IsNeg, resExp, finalM);
    }

    public static Float34 operator /(Float34 a, Float34 b)
    {
        if (b.IsZero) return CreateInf(a.IsNeg ^ b.IsNeg);
        int resExp = a.GetExp() - b.GetExp() + 255;
        byte[] rem = GetMantissaArray(a);
        byte[] div = GetMantissaArray(b);
        byte[] quot = new byte[4];

        for (int i = 30; i >= 0; i--)
        {
            if (Compare(rem, div) >= 0) { Subtract(rem, div, rem); SetBit(quot, i); }
            ShiftLeft(rem);
        }
        return Pack(a.IsNeg ^ b.IsNeg, resExp, quot);
    }

    public static Float34 Sqrt(Float34 a)
    {
        if (a.IsNeg || a.IsNaN) return CreateNaN();
        if (a.IsZero || a.IsInf) return a;

        int exp = a.GetExp();
        byte[] m = GetMantissaArray(a);
        if ((exp - 255) % 2 != 0) { ShiftLeft(m); exp--; } // Делаем степень четной

        int resExp = (exp - 255) / 2 + 255;
        byte[] resM = new byte[4];
        byte[] root = new byte[4];
        // Побитовый алгоритм корня (Digit-by-digit)
        for (int i = 30; i >= 0; i--)
        {
            byte[] temp = new byte[4];
            Array.Copy(root, temp, 4);
            SetBit(temp, i);
            if (Compare(m, temp) >= 0) { Subtract(m, temp, m); SetBit(root, i); }
            ShiftLeft(m);
        }
        return Pack(false, resExp, root);
    }

    // --- Низкоуровневые байтовые помощники ---
    private static void ShiftLeft(byte[] b)
    {
        int c = 0;
        for (int i = 3; i >= 0; i--) { int nc = (b[i] & 0x80) != 0 ? 1 : 0; b[i] = (byte)((b[i] << 1) | c); c = nc; }
    }

    private static void ShiftRight(byte[] b, int n)
    {
        for (int j = 0; j < n; j++)
        {
            int c = 0;
            for (int i = 0; i < 4; i++) { int nc = (b[i] & 1) << 7; b[i] = (byte)((b[i] >> 1) | c); c = nc; }
        }
    }

    private static bool Add(byte[] a, byte[] b, byte[] res)
    {
        int c = 0;
        for (int i = 3; i >= 0; i--) { int s = a[i] + b[i] + c; res[i] = (byte)s; c = s >> 8; }
        return c > 0;
    }

    private static void Subtract(byte[] a, byte[] b, byte[] res)
    {
        int br = 0;
        for (int i = 3; i >= 0; i--) { int s = a[i] - b[i] - br; if (s < 0) { res[i] = (byte)(s + 256); br = 1; } else { res[i] = (byte)s; br = 0; } }
    }

    private static void AddToLarge(byte[] target, byte[] add, int shift)
    {
        int c = 0; int offset = shift / 8;
        for (int i = 3; i >= 0; i--)
        {
            int pos = 7 - (3 - i) - offset;
            if (pos < 0) continue;
            int s = target[pos] + add[i] + c; target[pos] = (byte)s; c = s >> 8;
        }
    }

    private static bool GetBit(byte[] b, int n) => (b[3 - n / 8] & (1 << (n % 8))) != 0;
    private static void SetBit(byte[] b, int n) => b[3 - n / 8] |= (byte)(1 << (n % 8));
    private static int Compare(byte[] a, byte[] b)
    {
        for (int i = 0; i < 4; i++) { if (a[i] > b[i]) return 1; if (a[i] < b[i]) return -1; }
        return 0;
    }

    private static byte[] GetMantissaArray(Float34 f)
    {
        byte[] m = new byte[4];
        m[0] = (byte)(0x40 | (f.Data[1] & 0x3F)); m[1] = f.Data[2]; m[2] = f.Data[3]; m[3] = f.Data[4];
        return m;
    }

    private static Float34 Pack(bool sign, int exp, byte[] m)
    {
        Float34 r = new Float34(true);
        if (exp >= 0x1FF) return CreateInf(sign);
        if (exp <= 0) return r;
        r.Data[0] = (byte)((sign ? 0x80 : 0) | (exp >> 2));
        r.Data[1] = (byte)((exp << 6) | (m[0] & 0x3F));
        r.Data[2] = m[1]; r.Data[3] = m[2]; r.Data[4] = m[3];
        return r;
    }

    public static Float34 CreateNaN() => new Float34 { Data = new byte[] { 0x7F, 0xFF, 0xFF, 0xFF, 0xFF } };
    public static Float34 CreateInf(bool n) => new Float34 { Data = new byte[] { (byte)(n ? 0xFF : 0x7F), 0xC0, 0, 0, 0 } };

    public float ToFloat()
    {
        if (IsNaN) return float.NaN; if (IsInf) return IsNeg ? float.NegativeInfinity : float.PositiveInfinity;
        if (IsZero) return 0.0f;
        double mVal = 0; byte[] m = GetMantissaArray(this);
        for (int i = 0; i < 31; i++) if (GetBit(m, i)) mVal += Math.Pow(2, i - 30);
        return (float)((IsNeg ? -1 : 1) * mVal * Math.Pow(2, GetExp() - 255));
    }

    public static Float34 FromFloat(float f)
    {
        if (float.IsNaN(f)) return CreateNaN();
        if (float.IsInfinity(f)) return CreateInf(f < 0);
        if (f == 0) return new Float34(true);
        byte[] b = BitConverter.GetBytes(f);
        uint bits = BitConverter.ToUInt32(b, 0);
        bool s = (bits & 0x80000000) != 0;
        int e = (int)((bits >> 23) & 0xFF) + (255 - 127);
        uint m = (bits & 0x7FFFFF) << 7;
        byte[] mb = BitConverter.GetBytes(m); Array.Reverse(mb);
        return Pack(s, e, mb);
    }
}
