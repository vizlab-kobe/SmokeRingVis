#include <iostream>
#include <vector>
#include <limits>
#include <climits>
#include <set>
#include <stack>
#include <fstream>
#include <sstream>
#include <string>
#include <boost/filesystem.hpp>
#include <chrono>
#include <cmath> 

namespace fs = boost::filesystem;
using namespace std;

class Graph {
public:
    int V; 
    vector<vector<pair<int, float>>> adj; 

    Graph(int V) {
        this->V = V;
        adj.resize(V);
    }

    void addEdge(int u, int v, float weight) {
        adj[u].push_back(make_pair(v, weight));
    }

    void dijkstra(int start, vector<float>& dist, vector<int>& parent) {
        dist.assign(V, std::numeric_limits<float>::max());
        parent.assign(V, -1);
        dist[start] = 0.0f;

        set<pair<float, int>> s; // (distance, node)
        s.insert(make_pair(0.0f, start));
        int i=0;
        while (!s.empty()) {
            auto u = s.begin()->second;
            s.erase(s.begin());
            for (auto& neighbor : adj[u]) {
                int v = neighbor.first;
                float weight = neighbor.second;
                if (dist[u] + weight < dist[v]) {
                    if (dist[v] != std::numeric_limits<float>::max()) {
                        s.erase(make_pair(dist[v], v));
                    }
                    dist[v] = dist[u] + weight;
                    parent[v] = u;
                    s.insert(make_pair(dist[v], v));
                }
            }    

        }
    }

    stack<int> printPath(const vector<int>& parent, int node) {
        if (node == -1) return {};
        stack<int> path;
        int current = node;
        while (current != -1) {
            path.push(current);
            current = parent[current];
        }
        return path;
    }
};

std::vector<float> extractColumnFloat(const std::string& filename, size_t column_index) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Failed to open file: " << filename << std::endl;
    }

    std::vector<std::string> column_data; 
    std::string line;
    std::getline(file, line);
    while (std::getline(file, line)) {
        std::istringstream ss(line);
        std::string cell;
        size_t current_index = 0;

        while (std::getline(ss, cell, ',')) {
            if (current_index == column_index) {
                column_data.push_back(cell); 
                break; 
            }
            ++current_index;
        }
    }

    file.close();

    std::vector<float> vec; 
    for (auto value : column_data) {
        auto va = std::stof(value);
        vec.push_back( va );
    }
    return vec;
}

std::vector<std::string> extractColumn(const std::string& filename, size_t column_index) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Failed to open file: " << filename << std::endl;
    }

    std::vector<std::string> column_data; 
    std::string line;
    std::getline(file, line);
    while (std::getline(file, line)) {
        std::istringstream ss(line);
        std::string cell;
        size_t current_index = 0;

       
        while (std::getline(ss, cell, ',')) {
            if (current_index == column_index) {
                column_data.push_back(cell); 
                break; 
            }
            ++current_index;
        }
    }

    file.close();

    std::vector<std::string> vec; 
    for (auto value : column_data) {
        vec.push_back( value );
    }
    return vec;
}

std::vector<std::string> listFiles(const fs::path& directory) {
    try {
        if (!fs::exists(directory) || !fs::is_directory(directory)) {
            std::cerr << "Invalid directory: " << directory << std::endl;
            return{};
        }

        for (const auto& entry : fs::directory_iterator(directory)) {
            if (fs::is_regular_file(entry)) {
                std::cout << "File: " << entry.path().filename().string() << std::endl;
            } else if (fs::is_directory(entry)) {
                std::cout << "Directory: " << entry.path().filename().string() << std::endl;
            }
        }
    } catch (const fs::filesystem_error& e) {
        std::cerr << "Filesystem error: " << e.what() << std::endl;
    }
}

std::vector<int> getFilenameDetail(std::string filename){
    std::vector<int> file;
    size_t firstUnderscore = filename.find('_'); 
    size_t secondUnderscore = filename.find('_', firstUnderscore + 1); 
    size_t thirdUnderscore = filename.find('_', secondUnderscore + 1);
    size_t fourthUnderscore = filename.find('_', thirdUnderscore + 1);
    size_t periodScore = filename.find('.');

    int time = atoi((filename.substr(firstUnderscore + 1, secondUnderscore - firstUnderscore - 1)).c_str() );
    int candidate = atoi((filename.substr(secondUnderscore + 1, thirdUnderscore - secondUnderscore - 1)).c_str() );
    int zoomLevel = atoi((filename.substr(thirdUnderscore + 1, fourthUnderscore - thirdUnderscore - 1)).c_str() );
    int route = atoi((filename.substr(fourthUnderscore + 1, periodScore - fourthUnderscore - 1)).c_str() );
    file.push_back(time);
    file.push_back(candidate);
    file.push_back(zoomLevel);
    file.push_back(route);
    return file;
}

bool getRouteImage(int time_from, int time_to, int candidate_num, int from, int to, std::string filename ){
    int routeNum = (from%candidate_num)*candidate_num + to%candidate_num;
    size_t firstUnderscore = filename.find('_'); 
    size_t secondUnderscore = filename.find('_', firstUnderscore + 1);
    size_t thirdUnderscore = filename.find('_', secondUnderscore + 1);
    size_t fourthUnderscore = filename.find('_', thirdUnderscore + 1); 
    size_t periodScore = filename.find('.'); 
    std::string routeImage;

    int time = atoi( (filename.substr(firstUnderscore + 1, secondUnderscore - firstUnderscore - 1) ).c_str() );
    int route = atoi( (filename.substr(fourthUnderscore + 1, periodScore - fourthUnderscore - 1) ).c_str() );
    if(time > time_from && time < time_to){
        if(routeNum == route){
            return true;
        }
    }
    return false;
}

int main(int argc, char *argv[]) {
    chrono::system_clock::time_point start, end;

    std::vector<std::string> fn; 
    std::vector<float> en;
    std::vector<float> pFP;
    std::vector<float> pCP;
    std::string cell;
    stack<int> path;
    auto candidate_num = atoi(argv[1]);
    auto first_file = atoi(argv[2]);
    auto entropy_ratio = atof(argv[3]);
    auto focus_path_length_ratio = atof(argv[4]);
    auto camera_path_length_ratio = atof(argv[5]);
    std::ifstream file("./InSituVis.mpi.CameraFocusMulti/Output/output_video_params.csv");

    if (!file.is_open()) {
        std::cerr << "ファイルを開けませんでした。" << std::endl;
        return 1;
    }

    fn = extractColumn("./InSituVis.mpi.CameraFocusMulti/Output/output_video_params.csv", 0);
    en = extractColumnFloat("./InSituVis.mpi.CameraFocusMulti/Output/output_video_params.csv", 1);

    for (auto& val : en) {
        val = 1.0f / val;
    }
    Graph g(fn.size()); 
    std::string line;
    std::getline(file, line);
    while (std::getline(file, line)) {
         std::istringstream i_stream(line);
         int n = 0;
         while(getline(i_stream, cell, ',')){
            auto a = atof(cell.c_str());
            if(n>1 && 2+candidate_num>n) pFP.push_back(a);
            else if(n>1+candidate_num && 2*(candidate_num)+2>n) pCP.push_back(a);
            n++;
         }
    }
    file.clear();         
    file.seekg(0);        
    for(size_t i=0;i<candidate_num+1;i++){
        std::getline(file, line);
    }
    int check = 1; 
   for(size_t i=0; i<fn.size();i++){
        float sum_en = 0.0f;
        auto path = candidate_num*candidate_num*(check-1);
        float sum_pFP = 0.0f;
        float sum_pCP = 0.0f;

        for(size_t j=0; j<candidate_num;j++){
            sum_en = sum_en + en[check*candidate_num+j]; 
            sum_pFP = sum_pFP + pFP[path + candidate_num*j + i%candidate_num];
            sum_pCP = sum_pCP + pCP[path + candidate_num*j + i%candidate_num];
        }

        for(size_t j=0;j<candidate_num;j++){
            auto weight = entropy_ratio * en[check*candidate_num+j]/sum_en + focus_path_length_ratio * pFP[path + candidate_num*j + i%candidate_num]/sum_pFP + camera_path_length_ratio * pCP[path + candidate_num*j + i%candidate_num]/sum_pCP;
            g.addEdge(i,check*candidate_num+j,weight);
        }

        if(i%candidate_num == candidate_num-1) check++;
   } 

    vector<int> goals;
    for(int i=0; i<candidate_num; i++){
        goals.push_back(fn.size()-i-1);
    }
    vector<float> dist;
    vector<int> parent;
    start = chrono::system_clock::now();
    g.dijkstra(first_file, dist, parent);
    end = chrono::system_clock::now();
    double time = static_cast<double>(chrono::duration_cast<chrono::microseconds>(end - start).count() / 1000.0);
    printf("time %lf[ms]\n", time);
    float minDistance = std::numeric_limits<float>::max();
    int bestGoal = -1;
    for (int goal : goals) {
        // std::cout<<dist[goal]<<std::endl;
        if (dist[goal] < minDistance) {
            minDistance = dist[goal];
            bestGoal = goal;
        }
    }
    if (bestGoal != -1) {
    // cout << "最短距離のゴールノード: " << bestGoal << endl;
    // cout << "最短距離: " << minDistance << endl;
    // cout << "経路: ";
    path =  g.printPath(parent, bestGoal);
    } else {
        cout << "ゴールノードへの経路が見つかりません。" << endl;
    }

    std::string newDirPath = "./InSituVis.mpi.CameraFocusMulti/ex_Output/";
    std::string sourceImagePath;

    fs::create_directory(newDirPath);
    fs::create_directory(newDirPath + "Output/");
    std::string destImagePath = newDirPath+"Output/";
    int fromImageindex = 0;
    int count = 0;
    start = chrono::system_clock::now();
    while (!path.empty()) {
        sourceImagePath = "./InSituVis.mpi.CameraFocusMulti/"+fn[path.top()];
        destImagePath = "./InSituVis.mpi.CameraFocusMulti/ex_Output/" + fn[path.top()];
        fs::copy_file(sourceImagePath, destImagePath, fs::copy_option::overwrite_if_exists);
        auto fromTime = getFilenameDetail(fn[fromImageindex]);
        auto toTime = getFilenameDetail(fn[path.top()]);

        if (count>0){
            for (const auto& entry : fs::directory_iterator("./InSituVis.mpi.CameraFocusMulti/Output")) {
                if(getRouteImage(*fromTime.begin(),*toTime.begin(),candidate_num,fromImageindex,path.top(),entry.path().filename().c_str())){ 
                    auto f = "./InSituVis.mpi.CameraFocusMulti/Output/"+entry.path().filename().string();
                    auto t ="./InSituVis.mpi.CameraFocusMulti/ex_Output/Output/"+entry.path().filename().string();
                    fs::copy_file(f, t, fs::copy_option::overwrite_if_exists);
                }
            }            
        }
        fromImageindex = path.top();
        path.pop();
        count = count+1;
    }
    end = chrono::system_clock::now();
    time = static_cast<double>(chrono::duration_cast<chrono::microseconds>(end - start).count() / 1000.0);
    printf("time2 %lf[ms]\n", time);

    return 0;
}
