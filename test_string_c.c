#include "c-out/blitz_types.h"
#include <stdio.h>

// Implement print stubs
void print_str(const char* str) {
    printf("%s\n", str);
}

void print_int(int64_t val) {
    printf("%lld\n", (long long)val);
}

void print_list(List_Rune list) {
    printf("List_Rune{ len=%zu }\n", list.len);
}

int main() {
    const char* str = "hello";
    List_Rune chars = blitz_string_chars(str);
    size_t len = chars.len;
    
    print_str("String length:");
    print_int(len);
    
    char* sub = blitz_substring(chars, 1, 4);
    print_str("Substring (1, 4):");
    print_str(sub);
    
    free(chars.data);
    free(sub);
    
    return 0;
}
