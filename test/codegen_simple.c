#include <stdbool.h>

int f(int a, int b) {
    return a / b;
}

int main() {
    putchar(6 * 6 + 6);
    putchar(236 % 7 + 5214 / 140);
    putchar(f(85, 2));
    for (int i = 0; i < 3; i++) {
        putchar(42);
    }
    if (true) {
        putchar(42);
    } else {
        putchar(43);
    }
    putchar(10);
    return 0;
}
