#include <filesystem>
#include <fstream>
#include <string>
#include <iostream>
#include <stdexcept>
#include <algorithm>

namespace file = std::filesystem;
using path = file::path;

void cd_to_itest_src()
{
    if(file::current_path().stem().string() 
            == "itest_src") {
        return;
    }

    while(file::current_path().stem().string() 
            != "hascc") {
        auto to_change {file::current_path().parent_path()};
        file::current_path(to_change);
        if(file::current_path() 
                == file::current_path().root_path()) {
            throw std::runtime_error("Could not find hascc directory");
        }
    }

    file::current_path("integration/itest_src");
}


// must be in itest_src directory
std::vector<file::path> get_test_directories()
{
    if(file::current_path().stem().string() 
            != "itest_src") {
        throw std::runtime_error("Must be in itest_src directory to run");
    }
    file::directory_iterator 
            parent{absolute(file::current_path().parent_path())};

    std::vector<path> result;

    for(auto &item : parent) {
        if(file::is_directory(item) && 
                item.path().stem().string() != "itest_src" ) {
            result.push_back(item);
        }
    }

    std::sort(begin(result), end(result));
    return result; 
}


int main(int,char**)
{
    cd_to_itest_src();

    std::vector test_directories = get_test_directories();

    std::ofstream stream{"paths.txt"};
    for(auto &dir : test_directories) {
        if(dir.empty()) {
            continue;
        }
        auto p {file::directory_iterator
                    {file::absolute(dir)}};
        std::vector<path> files {};
        for(auto &f : p) {
            files.push_back(std::move(f));
        }
        std::sort(begin(files),end(files));
        for(auto &f : files) {
            stream << f.string() << " ";
        }
        stream << "\n";
    }
    return 0;
} 
