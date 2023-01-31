int main() {
    int f;
    void fib(int n) {
        void mksum() {
            fib(n - 2);
            int tmp = f;
            fib(n - 1);
            f = f + tmp;
        }
        if (n <= 1) f = n; else mksum();
    }
    fib(10);
    putchar(f);
    putchar(10);
    return 0;
}
