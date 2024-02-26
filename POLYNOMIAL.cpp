// Bartosz Bugajski

#include <iostream>
#include <cstdarg>
using namespace std;

int GCD(int i1, int i2)
{
    if(i1 == 0 && i2 != 0)
    {
        if(i2 < 0) return -i2;
        return i2;
    }
    else if(i2 == 0 && i1 != 0)
    {
        if(i1 < 0) return -i1;
        return i1;
    }
    else if(i1 == 0) return 0;
    int gcd = i1;
    int x = i2;
    if (i1 > i2)
    {
        gcd = i2;
        x = i1;
    }
    while (x % gcd != 0)
    {
        int temp = gcd;
        gcd = x % gcd;
        x = temp;
    }
    if(gcd < 0) gcd = -gcd;
    return gcd;
}

int LCM(int i1, int i2)
{
    if(GCD(i1, i2) != 0 ) return i1 * i2 / GCD(i1, i2);
    return 0;
}

class FRACTION
{
public:
    int numerator;
    int denom;

    FRACTION(int n = 0, int d = 1)
    {
        numerator = n;
        denom = d;
        setSign();
        Simplify();
    }

    FRACTION(const FRACTION& f)
    {
        numerator = f.numerator;
        denom = f.denom;
        setSign();
        Simplify();
    }

    void Simplify()
    {
        if (numerator == 0)
        {
            denom = 1;
            return;
        }
        int gcd = GCD(numerator, denom);
        if (gcd != 0)
        {
            numerator /= gcd;
            denom /= gcd;
        }
    }

    void setSign()
    {
        if (denom < 0)
        {
            numerator = -numerator;
            denom = -denom;
        }
    }

    FRACTION& operator= (const FRACTION& p)
    {
        if(this != &p)
        {
            numerator = p.numerator;
            denom = p.denom;
            Simplify();
            setSign();
        }
        return *this;
    }
};

FRACTION operator+ (const FRACTION& u1, const FRACTION& u2)
{
    int num = u1.numerator * u2.denom + u2.numerator * u1.denom;
    int den = u1.denom * u2.denom;
    FRACTION result(num, den);
    return result;
}

FRACTION operator- (const FRACTION& u1, const FRACTION& u2)
{
    int num = u1.numerator * u2.denom - u2.numerator * u1.denom;
    int den = u1.denom * u2.denom;
    FRACTION result(num, den);
    return result;
}

FRACTION operator* (const FRACTION& u1, const FRACTION& u2)
{
    int num = u1.numerator * u2.numerator;
    int den = u1.denom * u2.denom;
    FRACTION result(num, den);
    return result;
}

FRACTION operator/ (const FRACTION& u1, const FRACTION& u2)
{
    int num = u1.numerator * u2.denom;
    int den = u1.denom * u2.numerator;
    FRACTION result(num, den);
    return result;
}

class POLYNOMIAL;

bool operator< (const POLYNOMIAL&, const POLYNOMIAL&);

class POLYNOMIAL
{
public:
    int* coefficients;
    int degree;
    static int overloaded;

    POLYNOMIAL()
    {
        degree = 0;
        coefficients = new int[degree + 1];
        coefficients[0] = 0;
    }

    POLYNOMIAL(int d, int c1, ...)
    {
        degree = d;
        coefficients = new int[d + 1];
        coefficients[0] = c1;
        va_list args;
        va_start(args, c1);

        for (int i = 1; i < d + 1; i++)
        {
            coefficients[i] = va_arg(args, int);
        }

        va_end(args);
        removeZeros();
        Simplify();
    }

    POLYNOMIAL(const POLYNOMIAL& p)
    {
        degree = p.degree;
        coefficients = new int[degree + 1];
        for(int i = 0; i < degree + 1; i++)
        {
            coefficients[i] = p.coefficients[i];
        }
        removeZeros();
        Simplify();
    }

    ~POLYNOMIAL()
    {
        delete[] coefficients;
        coefficients = NULL;
    }

    POLYNOMIAL operator+ (const POLYNOMIAL& p2)
    {
        POLYNOMIAL p1 = *this;
        POLYNOMIAL help = p2;
        if(p2.degree > p1.degree)
        {
            help = p1;
            p1 = p2;
        }

        for (int i = 0; i < p1.degree + 1; i++)
        {
            if(i <= help.degree)
            {
                p1.coefficients[i] += help.coefficients[i];
            }
        }
        p1.removeZeros();
        p1.Simplify();
        return p1;
    }

    POLYNOMIAL operator- ()
    {
        POLYNOMIAL p1 = *this;
        for(int i = 0; i < p1.degree + 1; i++)
        {
            p1.coefficients[i] = -p1.coefficients[i];
        }
        return p1;
    }

    POLYNOMIAL operator- (const POLYNOMIAL& p2)
    {
        POLYNOMIAL p1 = *this;
        POLYNOMIAL help = p2;
        POLYNOMIAL p3 = p1 + (-help);
        return p3;
    }

    POLYNOMIAL operator* (const POLYNOMIAL& p2)
    {
        POLYNOMIAL p1 = *this;
        POLYNOMIAL p3;
        p3.degree = p1.degree + p2.degree;
        delete[] p3.coefficients;
        p3.coefficients = new int[p3.degree + 1];
        for(int i = 0; i < p3.degree + 1; i++)
        {
            p3.coefficients[i] = 0;
        }
        for(int i = 0; i < p1.degree + 1; i++)
        {
            for(int j = 0; j < p2.degree + 1; j++)
            {
                p3.coefficients[i + j] += p1.coefficients[i] * p2.coefficients[j];
            }
        }
        p3.removeZeros();
        p3.Simplify();
        return p3;
    }

    POLYNOMIAL operator/ (const POLYNOMIAL& p2)
    {
        if(p2.degree == 0 && p2.coefficients[0] == 0 || degree == 0 && coefficients[0] == 0 || degree < p2.degree)
        {
            return POLYNOMIAL();
        }
        POLYNOMIAL p1 = *this;
        FRACTION f1[p1.degree + 1];
        FRACTION f2[p2.degree + 1];
        FRACTION result[p1.degree - p2.degree + 1];

        for(int i = 0; i < p1.degree + 1; i++)
        {
            f1[i].numerator = p1.coefficients[i];
        }

        for(int i = 0; i < p2.degree + 1; i++)
        {
            f2[i].numerator = p2.coefficients[i];
        }
        int d = p1.degree;
        while(d >= p2.degree)
        {
            result[d - p2.degree] = f1[d] / f2[p2.degree];
            FRACTION temp[d + 1];
            for(int i = 0; i <= d; i++)
            {
                if(i >= d - p2.degree)
                {
                    temp[i] = f2[i - d + p2.degree] * result[d - p2.degree];
                }
            }
            for(int i = 0; i <= d; i++)
            {
                f1[i] = f1[i] - temp[i];
            }
            while(d >=0 && f1[d].numerator == 0)
            {
                d--;
            }
        }

        POLYNOMIAL p3;
        p3.degree = p1.degree - p2.degree;
        delete[] p3.coefficients;
        p3.coefficients = new int[p3.degree + 1];
        int lcm = result[0].denom;
        for(int i = 0; i < p3.degree + 1; i++)
        {
            lcm = LCM(lcm, result[i].denom);
        }
        for(int i = 0; i < p3.degree + 1; i++)
        {
            p3.coefficients[i] = result[i].numerator * lcm / result[i].denom;
        }
        p3.removeZeros();
        p3.Simplify();

        return p3;
    }

    POLYNOMIAL operator% (const POLYNOMIAL& p2)
    {
        if(p2.degree == 0 && p2.coefficients[0] == 0 || degree < p2.degree)
        {
            return *this;
        }
        if(p2.degree == 0) return POLYNOMIAL();

        POLYNOMIAL p1 = *this;
        FRACTION f1[p1.degree + 1];
        FRACTION f2[p2.degree + 1];

        for(int i = 0; i < p1.degree + 1; i++)
        {
            f1[i].numerator = p1.coefficients[i];
        }
        for(int i = 0; i < p2.degree + 1; i++)
        {
            f2[i].numerator = p2.coefficients[i];
        }
        int d = p1.degree;
        while(d >= p2.degree)
        {
            FRACTION temp[d + 1];
            for(int i = 0; i <= d; i++)
            {
                if(i >= d - p2.degree)
                {
                    temp[i] = f2[i - d + p2.degree] * f1[d] / f2[p2.degree];
                }
            }
            for(int i = 0; i <= d; i++)
            {
                f1[i] = f1[i] - temp[i];
            }
            while(d >=0 && f1[d].numerator == 0)
            {
                d--;
            }
        }

        POLYNOMIAL p3;
        p3.degree = p2.degree - 1;
        delete[] p3.coefficients;
        p3.coefficients = new int[p3.degree + 1];
        int lcm = f1[0].denom;
        for(int i = 0; i < p3.degree + 1; i++)
        {
            lcm = LCM(lcm, f1[i].denom);
        }
        for(int i = 0; i < p3.degree + 1; i++)
        {
            p3.coefficients[i] = f1[i].numerator * lcm / f1[i].denom;
        }
        p3.removeZeros();
        p3.Simplify();
        return p3;
    }

    POLYNOMIAL operator<< (int x)
    {
        if(x > degree)
        {
            POLYNOMIAL p;
            return p;
        }
        POLYNOMIAL p1 = *this;
        POLYNOMIAL result;
        if(x < 0)
        {
            return result;
        }
        delete[] result.coefficients;
        result.degree = p1.degree - x;
        result.coefficients = new int[result.degree + 1];
        for(int j = 0; j < result.degree + 1; j++)
        {
            result.coefficients[j] = p1.coefficients[j + x];
        }
        result.removeZeros();
        result.Simplify();
        return result;
    }

    POLYNOMIAL operator>> (int x)
    {
        POLYNOMIAL p1 = *this;
        POLYNOMIAL result;
        if(x < 0)
        {
            return result;
        }
        delete[] result.coefficients;
        result.degree = p1.degree + x;
        result.coefficients = new int[result.degree + 1];
        for(int i = 0; i < result.degree + 1; i++)
        {
            if(i < x) result.coefficients[i] = 0;
            else result.coefficients[i] = p1.coefficients[i - x];
        }
        result.removeZeros();
        return result;
    }

    POLYNOMIAL& operator-- ()
    {
        POLYNOMIAL& p1 = *this;
        for(int i = 0; i < p1.degree + 1; i++)
        {
            p1.coefficients[i] -= 1;
        }
        p1.removeZeros();
        p1.Simplify();
        return p1;
    }

    POLYNOMIAL operator-- (int)
    {
        POLYNOMIAL& p1 = *this;
        POLYNOMIAL p2 = p1;
        for(int i = 0; i < p1.degree + 1; i++)
        {
            p1.coefficients[i] -= 1;
        }
        p1.removeZeros();
        p1.Simplify();
        return p2;
    }

    POLYNOMIAL& operator++ ()
    {
        POLYNOMIAL& p1 = *this;
        for(int i = 0; i < p1.degree + 1; i++)
        {
            p1.coefficients[i] += 1;
        }
        p1.removeZeros();
        p1.Simplify();
        return p1;
    }

    POLYNOMIAL operator++ (int)
    {
        POLYNOMIAL& p1 = *this;
        POLYNOMIAL p2 = p1;
        for(int i = 0; i < p1.degree + 1; i++)
        {
            p1.coefficients[i] += 1;
        }
        p1.removeZeros();
        p1.Simplify();
        return p2;
    }

    void Simplify()
    {
        if(degree == 0 && coefficients[0] != 0)
        {
            if(coefficients[0] < 0) coefficients[0] = -1;
            else coefficients[0] = 1;
        }
        else if(degree != 0)
        {
            int gcd = GCD(coefficients[0], coefficients[1]);
            for (int i = 2; i < degree + 1; i++)
            {
                gcd = GCD(gcd, coefficients[i]);
            }

            if(gcd != 0)
            {
                for (int i = 0; i < degree + 1; i++)
                {
                    coefficients[i] /= gcd;
                }
            }
        }
    }

    void removeZeros()
    {
        int newdegree = 0;
        for(int i = degree; i >= 0; i--)
        {
            if(coefficients[i] != 0)
            {
                newdegree = i;
                break;
            }
        }
        int newcoeff[newdegree + 1];
        for(int i = 0; i < newdegree + 1; i++)
        {
            newcoeff[i] = coefficients[i];
        }
        delete[] coefficients;
        coefficients = new int[newdegree + 1];
        for(int i = 0; i < newdegree + 1; i++)
        {
            coefficients[i] = newcoeff[i];
        }
        degree = newdegree;
    }

    POLYNOMIAL& operator= (const POLYNOMIAL& p)
    {
        if(this != &p)
        {
            delete[] coefficients;
            degree = p.degree;
            coefficients = new int[degree + 1];
            for(int i = 0; i < degree + 1; i++)
            {
                coefficients[i] = p.coefficients[i];
            }
            removeZeros();
            Simplify();
        }
        return *this;
    }

    void* operator new(size_t amount)
    {
        overloaded++;
        return ::operator new(amount);
    }

    void operator delete(void* pointer)
    {
        overloaded--;
        ::operator delete(pointer);
    }

    void* operator new[](size_t amount)
    {
        overloaded++;
        return ::operator new(amount);
    }

    void operator delete[](void* pointer)
    {
        overloaded--;
        ::operator delete(pointer);
    }

    POLYNOMIAL& operator+= (const POLYNOMIAL& p2)
    {
        *this = *this + p2;
        return *this;
    }
    POLYNOMIAL& operator-= (const POLYNOMIAL& p2)
    {
        *this = *this - p2;
        return *this;
    }
    POLYNOMIAL& operator*= (const POLYNOMIAL& p2)
    {
        *this = *this * p2;
        return *this;
    }
    POLYNOMIAL& operator/= (const POLYNOMIAL& p2)
    {
        *this = *this / p2;
        return *this;
    }
    POLYNOMIAL& operator<<= (const int x)
    {
        *this = *this << x;
        return *this;
    }
    POLYNOMIAL& operator>>= (const int x)
    {
        *this = *this >> x;
        return *this;
    }
    POLYNOMIAL& operator%= (const POLYNOMIAL& p2)
    {
        *this = *this % p2;
        return *this;
    }
};

ostream& operator<< (ostream& stream, const POLYNOMIAL& p)
{
    stream << "( ";
    for (int i = 0; i < p.degree + 1; i++)
    {
        stream << p.coefficients[i];
        if (i != p.degree) stream << ", ";
    }
    stream << " )";
    return stream;
}

istream& operator>> (istream& stream, POLYNOMIAL& p)
{
    stream >> p.degree;
    delete[] p.coefficients;
    p.coefficients = new int[p.degree + 1];
    for (int i = 0; i < p.degree + 1; i++)
    {
        stream >> p.coefficients[i];
    }
    p.removeZeros();
    p.Simplify();
    return stream;
}

bool operator< (const POLYNOMIAL& p1, const POLYNOMIAL& p2)
{
    if(p1.degree > p2.degree) return false;
    else if(p1.degree < p2.degree) return true;
    for(int i = p1.degree; i >= 0; i--)
    {
        if(p1.coefficients[i] < p2.coefficients[i]) return true;
        else if(p1.coefficients[i] > p2.coefficients[i]) return false;
    }
    return false;
}

bool operator!= (const POLYNOMIAL& p1, const POLYNOMIAL& p2)
{
    if(p1.degree != p2.degree) return true;
    for(int i = p1.degree; i >= 0; i--)
    {
        if(p1.coefficients[i] != p2.coefficients[i]) return true;
    }
    return false;
}

bool operator> (const POLYNOMIAL& p1, const POLYNOMIAL& p2)
{
    return !(p1 < p2) && p1 != p2;
}

bool operator<= (const POLYNOMIAL& p1, const POLYNOMIAL& p2)
{
    return !(p1 > p2);
}

bool operator>= (const POLYNOMIAL& p1, const POLYNOMIAL& p2)
{
    return !(p1 < p2);
}

bool operator== (const POLYNOMIAL& p1, const POLYNOMIAL& p2)
{
    return !(p1 != p2);
}