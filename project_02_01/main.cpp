#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <iomanip>
#include <sstream>
// #include <chrono>

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

    bool operator==(const BigInt& other) const {
        return limbs == other.limbs;
    }

    bool operator!=(const BigInt& other) const {
        return !(*this == other);
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

    // assume *this >= other
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

    pair<BigInt, BigInt> divMod(const BigInt& divisor) const {
        if (divisor.isZero()) return {BigInt(0), BigInt(0)}; 
        if (*this < divisor) return {BigInt(0), *this};
        
        BigInt quotient;
        quotient.limbs.resize(limbs.size());
        BigInt currentRemainder(0);

        for (int i = limbs.size() - 1; i >= 0; i--) {
            currentRemainder.shiftLeft32();
            if (currentRemainder.isZero()) currentRemainder.limbs.push_back(limbs[i]);
            else currentRemainder.limbs[0] = limbs[i];
            currentRemainder.normalize(); 

            // Binary Search for quotient digit
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
            
            quotient.limbs[i] = q;
            currentRemainder = currentRemainder - (divisor * BigInt(q));
        }
        quotient.normalize();
        return {quotient, currentRemainder};
    }

    BigInt operator/(const BigInt& other) const {
        return divMod(other).first;
    }

    BigInt operator%(const BigInt& other) const {
        return divMod(other).second;
    }
};

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

BigInt power(BigInt base, BigInt exp, BigInt mod) {
    BigInt res(1);
    base = base % mod;
    while (!exp.isZero()) {
        if ((exp.limbs[0] & 1)) { 
            res = (res * base) % mod;
        }
        base = (base * base) % mod;
        
        // exp = exp / 2
        uint32_t carry = 0;
        for (int i = exp.limbs.size() - 1; i >= 0; i--) {
            uint64_t cur = ((uint64_t)carry << 32) | exp.limbs[i];
            exp.limbs[i] = (uint32_t)(cur / 2);
            carry = (uint32_t)(cur % 2);
        }
        exp.normalize();
    }
    return res;
}

int main(int argc, char* argv[]) {
    if (argc < 3) {
        cerr << "Loi: Thieu tham so!" << endl;
        cerr << "Cach dung: " << argv[0] << " <input_file> <output_file>" << endl;
        return 1;
    }

    // auto start_time = chrono::high_resolution_clock::now();
    string inputPath = argv[1];
    string outputPath = argv[2];

    ifstream inpFile(inputPath);
    if (!inpFile) {
        cerr << "Loi: Khong the mo file input: " << inputPath << endl;
        return 1;
    }

    ofstream outFile(outputPath);
    if (!outFile) {
        cerr << "Loi: Khong the mo file output: " << outputPath << endl;
        return 1;
    }

    string lineP, lineN, lineG;

    
    // p
    if (!(inpFile >> lineP)) return 0;
    BigInt p = parseHexLittleEndian(lineP);
    
    // n 
    if (!(inpFile >> lineN)) return 0;
    BigInt n = parseHexLittleEndian(lineN);

    // set factors
    vector<BigInt> factors;
    BigInt zero(0);
    BigInt one(1);
    
    while (n > zero) {
        string s;
        if (!(inpFile >> s)) break; 
        factors.push_back(parseHexLittleEndian(s));
        n = n - one;
    }

    // g
    if (!(inpFile >> lineG)) return 0;
    BigInt g = parseHexLittleEndian(lineG);

    // Kiem tra can nguyen thuy
    BigInt pMinus1 = p - one;
    bool isPrimitive = true;
    
    if (g.isZero()) {
         isPrimitive = false;
    } else {
        for (const auto& k : factors) {
            BigInt exp = pMinus1 / k;
            BigInt check = power(g, exp, p);
            
            if (check == one) {
                isPrimitive = false;
                break;
            }
        }
    }

    if (isPrimitive) {
        outFile << "1";
    } else {
        outFile << "0";
    }
    inpFile.close();
    outFile.close();

    // auto end_time = std::chrono::high_resolution_clock::now();
    // chrono::duration<double> elapsed = end_time - start_time;

    // cout << "Test " << inputPath << ": " 
    //      << fixed << setprecision(4) << elapsed.count() << "s" << endl;

    return 0;
}