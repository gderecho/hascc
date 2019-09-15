#include <iostream>
#include "generate_paths.h"
#include "test_case.h"

int main(int,char**)
{
    auto paths {generate_paths()};
    int counter {0};
    int success {0};
    for(auto &pp : paths) {
        std::cout << "Test " << counter << ": "
            << pp.first.stem().string()
            << "....    ";
        TestCase t{
            std::move(pp.first),
            std::move(pp.second)};
        std::flush(std::cout);
        if(t.run()) {
            ++success;
            std::cout << "          Success.";
        } else {
            std::cout << "          Failure.";
        }
        ++counter;
        std::cout << "\n";
    }

    std::cout << success
        << " successes out of "
        << counter << " tests\n";
    return 0;
}
