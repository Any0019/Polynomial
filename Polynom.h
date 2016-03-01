#include <algorithm>
#include <iostream>
#include <vector>

template <typename Num_type> class Polynom {
public:
    std::vector<Num_type> coefs;

    Polynom(int n) {
        coefs.resize(n + 1, 0);
    }

    Polynom() {
        coefs = {};
    }

    int size() {
        return coefs.size();
    }

    Num_type count(Num_type k) {
        Num_type sum = 0;
        for (int i = 0; i < coefs.size(); ++i)
            sum += coefs[i] * power(k, i);
        return sum;
    }

    void operator= (Polynom& Poly2) {
        coefs = Poly2.coefs;
    }

    Num_type &operator[] (int n) {
        return coefs[n];
    }

    Polynom operator+ (Polynom& poly2) {
        int n = coefs.size(), m = poly2.size(), max = n;
        if (n < m)
            max = m;
        Polynom poly_rez(max - 1);
        int x = 0;
        while (x < n + m - max) {
            poly_rez[x] = coefs[x] + poly2[x];
            ++x;
        }
        while (x < max) {
            if (max == n)
                poly_rez[x] = coefs[x];
            else
                poly_rez[x] = poly2[x];
            ++x;
        }
        return poly_rez;
    }
    
    Polynom operator- (Polynom& poly2) {
        int n = coefs.size(), m = poly2.size(), max = n;
        if (n < m)
            max = m;
        Polynom poly_rez(max - 1);
        int x = 0;
        while (x < n + m - max) {
            poly_rez[x] = coefs[x] - poly2[x];
            ++x;
        }
        while (x < max) {
            if (max == n)
                poly_rez[x] = coefs[x];
            else
                poly_rez[x] = -poly2[x];
            ++x;
        }
        return poly_rez;
    }

    Polynom operator* (Polynom& poly2) {
        int n = coefs.size() - 1, m = poly2.size() - 1;
        Polynom poly_rez(m + n);
        for (int x = 0; x <= n; ++x)
            for (int y = 0; y <= m; ++y)
                poly_rez[x + y] += coefs[x] * poly2[y];
        return poly_rez;
    }

    Polynom operator* (Num_type k) {
        Polynom poly_rez(coefs.size() - 1);
        for (int x = 0; x < coefs.size(); ++x)
            poly_rez[x] = k * coefs[x];
        return poly_rez;
    }

    std::pair<Polynom, Polynom> operator/ (Polynom& poly2) {
        int n = coefs.size() - 1, m = poly2.size() - 1;
        Polynom poly_div(n - m), poly_mod(m), poly_now;
        poly_now.coefs = coefs;
        for (int x = n - m; x >= 0; --x) {
            Polynom poly2_on_mn(n);
            Num_type mn = poly_now[x + m] / poly2[m];
            for (int i = n; i >= n - m; --i) 
                poly2_on_mn[i - n + m + x] = poly2[i - n + m] * mn;
            poly_now = poly_now - poly2_on_mn;
            poly_div[x] = mn;
        }
        for (int x = 0; x <= m; ++x)
            poly_mod[x] = poly_now[x];
        return std::make_pair(poly_div, poly_mod);
    }

    Polynom div(Polynom& poly2) {
        Polynom poly1(coefs.size() - 1);
        std::pair<Polynom, Polynom> poly_rez;
        poly1.coefs = coefs;
        poly_rez = poly1 / poly2;
        return poly_rez.first;
    }

    Polynom mod(Polynom& poly2) {
        Polynom poly1(coefs.size() - 1);
        std::pair<Polynom, Polynom> poly_rez;
        poly1.coefs = coefs;
        poly_rez = poly1 / poly2;
        return poly_rez.second;
    }

    template <typename Num_type>
    friend std::ostream& operator<< (std::ostream& out, Polynom<Num_type>& poly);
    
    template <typename Num_type>
    friend std::istream& operator>> (std::istream& in, Polynom<Num_type>& poly);

private:

    Num_type power(Num_type a, int b) {
        if (b == 0) {
            return 1;
        } else if (b > 0) {
            Num_type rez = a;
            for (int x = 1; x < b; ++x)
                rez *= a;
            return rez;
        }

    }

};

template <typename Num_type>
std::ostream& operator<< (std::ostream& out, Polynom<Num_type>& poly) {
    int i1 = 0, i2 = poly.size() - 1;
    while (i1 < poly.size()) {
        if (poly[i1] != 0)
            break;
        ++i1;
    }
    while (i2 >= 0) {
        if (poly[i2] != 0)
            break;
        --i2;
    }
    for (int x = i2; x >= i1; --x) {
        if (poly[x] != 0) {
            if ((poly[x] != 1) && (poly[x] != -1)) {
                if ((x == i2) && (x > 0))
                    out << poly[x];
                else
                    if (x > 0)
                        out << abs(poly[x]);
            } else if (poly[x] == -1) {
                out << " - ";
            }
            if (x > 1) {
                if ( (poly[x] != 1) && (poly[x] != -1) )
                    out << "*x^" << x;
                else
                    out << "x^" << x;
            } else if (x == 1) {
                if ((poly[x] != 1) && (poly[x] != -1))
                    out << "*x";
                else
                    out << "x";
            } else if (x == 0) {
                out << abs(poly[x]);
            }
            if (x != i1)
                if (x > 0) {
                    if (poly[x - 1] > 0)
                        out << " + ";
                    else if ( (poly[x - 1] < 0) && (poly[x - 1] != -1) )
                        out << " - ";
                }
        } else {
                if (poly[x - 1] > 0)
                    out << " + ";
                else if ( (poly[x - 1] < 0) && (poly[x - 1] != -1) )
                    out << " - ";
        }
    }
    if (i1 == poly.size())
        out << 0;
    return out;
}


template <typename Num_type> 
std::istream& operator>> (std::istream& in, Polynom<Num_type>& poly) {
    if (poly.size() == 0)
        return in;
    auto it = poly.coefs.end();
    --it;
    for (it; it != poly.coefs.begin(); --it)
        in >> *it;
    in >> *it;
    return in;
}

