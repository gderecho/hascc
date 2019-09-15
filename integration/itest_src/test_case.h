#ifndef TEST_CASE_H_
#define TEST_CASE_H_

#include <filesystem>

class TestCase {
private:
    using path = std::filesystem::path;

    path srcfile_;
    path expfile_;

public:
    TestCase(path, path);
    ~TestCase();
    
    bool run();

    TestCase() = delete;
    TestCase(TestCase &&) = delete;
    TestCase(const TestCase &) = delete;
    TestCase &operator=(const TestCase &) = delete;
    TestCase &operator=(TestCase &&) = delete;

};

#endif
