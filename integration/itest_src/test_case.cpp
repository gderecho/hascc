#include <fstream>
#include <string>
#include <unordered_map>
#include <iostream>
#include <sys/wait.h>
#include "test_case.h"

namespace {

const std::string asmfile = "main.asm";
const std::string ofile = "main.o";
const std::string exefile = "main";

namespace f = std::filesystem;
using path = f::path;
using mss = std::unordered_map<
                std::string,std::string>;


mss read_file(path p)
{
    mss result;
    std::ifstream s{p.string()};
    while(!s.eof()) {
        std::string a,b;
        s >> a >> b;
        result.insert(std::make_pair(
                    std::move(a),std::move(b)));
    }
    return result;
}

} // namespace



TestCase::TestCase(path src,
        path exp)
    : srcfile_{src}, expfile_{exp} 
{}

TestCase::~TestCase()
{
    std::vector<std::string> to_delete 
        {asmfile,ofile,exefile};
    for(const auto &str : to_delete) {
        if(f::exists(path(str))) {
            f::remove(path(str));
        }
    }
}


bool TestCase::run()
{
    mss exp {read_file(expfile_)};

    std::string to_execute {"cat " 
        + srcfile_.string()
        + " | stack run > main.asm"
    };
    int ret {system(to_execute.c_str())};
    if(ret != 0) {
        if(exp.find("compile") != exp.end()) {
            if(exp["compile"] == std::string("false")) {
                return true;
            }
            std::cout << "it is " << exp["compile"] << "\n";
        }
        std::cerr << "Failed to compile\n";
        return false;
    }

    to_execute = "nasm -felf64 main.asm && gcc main.o -o main";
    ret = system(to_execute.c_str());
    if(ret != 0) {
        std::cerr << "Failed to assemble/link.\n" ;
        return false;
    }

    to_execute = "./main";
    ret = system(to_execute.c_str());
    if(exp.find("return_code") != end(exp)
            && exp["return_code"] != std::to_string(WEXITSTATUS(ret))) {
        std::cerr << "Wrong return code\n";
        std::cerr << "   expected: " << exp["return_code"] << "\n";
        std::cerr << "   actual: " << ret << "\n";
        return false;
    }

    return true;
}
