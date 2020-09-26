#include <iostream>
#include <random>

int main() {
    std::random_device r;
    std::default_random_engine el(r());
    std::uniform_int_distribution<int> dist(0, 15);

    char c;
    while (std::cin.get(c)) {
        if (c == '0') {
            if (!dist(el)) c = '1';
        } else if (c == '1') {
            if (!dist(el)) c = '0';
        }
        std::cout << c;
    }
    return 0;
}
