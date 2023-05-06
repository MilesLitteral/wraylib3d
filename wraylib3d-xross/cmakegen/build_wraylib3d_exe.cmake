cmake_minimum_required(VERSION 3.23)
project(WRL3dGame LANGUAGES CXX)

if (MSVC)
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
endif()

# This would Build wraylib3d.dll, it's not necessary, as GHC
# Does this for us, link to it instead.
# add_library(wraylib3d SHARED)
# target_include_directories(wraylib3d PRIVATE "${PROJECT_SOURCE_DIR}")
# add_subdirectory("cbits")

add_library(vulkan    SHARED IMPORTED [GLOBAL])
add_library(wraylib3d SHARED IMPORTED [GLOBAL])
#add_library(sdl2    SHARED IMPORTED [GLOBAL]) #TBA

add_executable(app)
target_sources(app PRIVATE "app/hs2c.cpp") #See: Notes on Haskell Via C++
                                              #tl;dr, a small interface needs to be made 
                                              #that runs app.hs in c++ (app.cpp reflection)
target_link_libraries(app PUBLIC vulkan)
target_link_libraries(app PUBLIC wraylib3d)

# you could install wraylib3d but this assumes 
# it was built by this same project 
# install(TARGETS wraylib3d FILE_SET HEADERS)