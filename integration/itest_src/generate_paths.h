#ifndef GENERATE_PATHS_H_
#define GENERATE_PATHS_H_

#include <vector>
#include <utility>
#include <filesystem>

std::vector<std::pair<
    std::filesystem::path,
    std::filesystem::path
    >> generate_paths();


#endif // GENERATE_PATHS_H_

