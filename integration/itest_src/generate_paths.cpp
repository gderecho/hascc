#include <filesystem>
#include <fstream>
#include <string>
#include <iostream>
#include <stdexcept>
#include <algorithm>
#include "generate_paths.h"

namespace {

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
std::vector<path> get_test_directories()
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

// processes an array of paths, representing
// the source and expectation files
void check_valid(
        const std::vector<path> &files, 
        const path &dir) 
{
    if(files.size() != 2) {
        throw std::runtime_error("Not two files in " 
                + dir.string());
    }

    if(files[0].extension() != ".c") {
        throw std::runtime_error("Source file does " 
                "not end in .c: "+ files[0].string());
    }

    if(files[1].extension() != ".json") {
        throw std::runtime_error("Source file does " 
                "not end in .json: "+ files[1].string());
    }
}


} // namespace

std::vector<std::pair<path,path>> generate_paths()
{
    cd_to_itest_src();

    std::vector<path> test_directories
            {get_test_directories()};

    std::vector<std::pair<path,path>> result {};

    for(auto &dir : test_directories) {

        if(file::is_empty(dir)) {
            continue;
        }

        auto p {file::directory_iterator
                    {file::absolute(dir)}};
        std::vector<path> files {};
        for(auto &f : p) {
            files.push_back(f);
        }

        sort(begin(files),end(files));
        check_valid(files,dir);

        result.emplace_back(
                std::make_pair(files[0],files[1]));
    }
    return result;
}
