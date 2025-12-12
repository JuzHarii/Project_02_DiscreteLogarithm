#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <iomanip>
#include <sstream>

using namespace std;

const uint64_t BASE = 0x100000000ULL;

class BigInt {
public:
    vector<uint32_t> limbs;

    BigInt() {}

    BigInt(uint64_t val) {
        if (val == 0) {
        } else if (val <= 0xFFFFFFFF) {
            limbs.push_back(static_cast<uint32_t>(val));
        } else {
            limbs.push_back(static_cast<uint32_t>(val & 0xFFFFFFFF));
            limbs.push_back(static_cast<uint32_t>(val >> 32));
        }
    }

    void normalize() {
        while (!limbs.empty() && limbs.back() == 0) {
            limbs.pop_back();
        }
    }

    bool isZero() const {
        return limbs.empty();
    }

    bool operator<(const BigInt& other) const {
        if (limbs.size() != other.limbs.size()) {
            return limbs.size() < other.limbs.size();
        }
        for (int i = limbs.size() - 1; i >= 0; i--) {
            if (limbs[i] != other.limbs[i]) {
                return limbs[i] < other.limbs[i];
            }
        }
        return false;
    }
    bool operator>(const BigInt& other) const { return other < *this; }
    bool operator<=(const BigInt& other) const { return !(*this > other); }
    bool operator>=(const BigInt& other) const { return !(*this < other); }
    bool operator==(const BigInt& other) const { return limbs == other.limbs; }
    bool operator!=(const BigInt& other) const { return !(*this == other); }

    BigInt operator+(const BigInt& other) const {
        BigInt res;
        size_t n = max(limbs.size(), other.limbs.size());
        res.limbs.resize(n);
        uint64_t carry = 0;
        for (size_t i = 0; i < n; i++) {
            uint64_t sum = carry;
            if (i < limbs.size()) sum += limbs[i];
            if (i < other.limbs.size()) sum += other.limbs[i];
            res.limbs[i] = static_cast<uint32_t>(sum & 0xFFFFFFFF);
            carry = sum >> 32;
        }
        if (carry) res.limbs.push_back(static_cast<uint32_t>(carry));
        return res;
    }

    BigInt operator-(const BigInt& other) const {
        BigInt res;
        res.limbs.resize(limbs.size());
        int64_t borrow = 0;
        for (size_t i = 0; i < limbs.size(); i++) {
            int64_t sub = limbs[i];
            if (i < other.limbs.size()) sub -= other.limbs[i];
            sub -= borrow;
            if (sub < 0) {
                sub += BASE;
                borrow = 1;
            } else {
                borrow = 0;
            }
            res.limbs[i] = static_cast<uint32_t>(sub);
        }
        res.normalize();
        return res;
    }

    BigInt operator*(const BigInt& other) const {
        if (isZero() || other.isZero()) return BigInt(0);
        BigInt res;
        res.limbs.resize(limbs.size() + other.limbs.size(), 0);
        
        for (size_t i = 0; i < limbs.size(); i++) {
            uint64_t carry = 0;
            for (size_t j = 0; j < other.limbs.size(); j++) {
                uint64_t cur = res.limbs[i+j] + (uint64_t)limbs[i] * other.limbs[j] + carry;
                res.limbs[i+j] = static_cast<uint32_t>(cur & 0xFFFFFFFF);
                carry = cur >> 32;
            }
            res.limbs[i + other.limbs.size()] += static_cast<uint32_t>(carry);
        }
        res.normalize();
        return res;
    }

    void shiftLeft32() {
        if (isZero()) return;
        limbs.insert(limbs.begin(), 0);
    }

    BigInt operator%(const BigInt& divisor) const {
        if (divisor.isZero()) return BigInt(0);
        if (*this < divisor) return *this;
        
        BigInt currentRemainder(0);
        for (int i = limbs.size() - 1; i >= 0; i--) {
            currentRemainder.shiftLeft32();
            if (currentRemainder.isZero()) currentRemainder.limbs.push_back(limbs[i]);
            else currentRemainder.limbs[0] = limbs[i];
            currentRemainder.normalize(); 

            uint64_t low = 0, high = 0xFFFFFFFF;
            uint32_t q = 0;
            
            while (low <= high) {
                uint64_t mid = low + (high - low) / 2;
                BigInt prod = divisor * BigInt(mid);
                if (prod <= currentRemainder) {
                    q = (uint32_t)mid;
                    low = mid + 1;
                } else {
                    high = mid - 1;
                }
            }
            if (q > 0) {
                 currentRemainder = currentRemainder - (divisor * BigInt(q));
            }
        }
        return currentRemainder;
    }
};

// doc hex little endian
BigInt parseHexLittleEndian(string s) {
    reverse(s.begin(), s.end());
    BigInt res;
    int len = s.length();
    for (int i = len; i > 0; i -= 8) {
        int start = max(0, i - 8);
        int count = i - start;
        string chunk = s.substr(start, count);
        uint32_t val = stoul(chunk, nullptr, 16);
        res.limbs.push_back(val);
    }
    res.normalize();
    return res;
}

// luy thua modulo
BigInt power(BigInt base, BigInt exp, BigInt mod) {
    BigInt res(1);
    base = base % mod;
    for (size_t i = 0; i < exp.limbs.size(); i++) {
        uint32_t limb = exp.limbs[i];
        for (int bit = 0; bit < 32; bit++) {
            if ((limb >> bit) & 1) {
                res = (res * base) % mod;
            }
            if (i == exp.limbs.size() - 1 && limb == 0) break;
            base = (base * base) % mod;
        }
    }
    return res;
}

int main(int argc, char* argv[]) {
    if (argc < 3) {
        cerr << "Usage: " << argv[0] << " <input file> <output file>" << endl;
        return 1;
    }

    string inputPath = argv[1];
    string outputPath = argv[2];

    ifstream fin(inputPath);
    if (!fin) {
        cerr << "khong mo duoc file input" << endl;
        return 1;
    }
    ofstream fout(outputPath);
    if (!fout) {
        cerr << "khong mo duoc file output" << endl;
        return 1;
    }

    string sP, sG, sY, sM, sR, sH;

    if (fin >> sP >> sG >> sY >> sM >> sR >> sH) {
        BigInt p = parseHexLittleEndian(sP);
        BigInt g = parseHexLittleEndian(sG);
        BigInt y = parseHexLittleEndian(sY);
        BigInt m = parseHexLittleEndian(sM);
        BigInt r = parseHexLittleEndian(sR);
        BigInt h = parseHexLittleEndian(sH);

        bool isValid = true;
        BigInt zero(0);
        BigInt p_minus_1 = p - BigInt(1);

        // check if 0 < r < p
        if (r.isZero() || r >= p) {
            isValid = false;
        }
        // check if 0 < h < p - 1
        if (h.isZero() || h >= p_minus_1) {
            isValid = false;
        }

        if (isValid) {
            // ve trai = g^m mod p
            BigInt left = power(g, m, p);

            // ve phai = (y^r * r^h) mod p
            BigInt yr = power(y, r, p);
            BigInt rh = power(r, h, p);
            BigInt right = (yr * rh) % p; // ve phai

            // so sanh 2 ve
            if (left != right) {
                isValid = false;
            }
        }
        if (isValid) {
            fout << "1";
        } else {
            fout << "0";
        }
    }
    fin.close();
    fout.close();

    return 0;
}