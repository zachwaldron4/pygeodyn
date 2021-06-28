#include <pybind11/embed.h> // everything needed for embedding
#include <pybind11/stl.h>
namespace py = pybind11;
using namespace pybind11::literals;
using namespace std;

#include <iostream>
#include <list>
#include <string>
#include <map>


void funcdir(){
    cout << "Possible functions in SatelliteFlythrough module are:" << "\n";
    cout << "ModelVariables, SampleTrajectory, ModelFlythrough, FakeFlight, and RealFlight (later)." << "\n";
    cout << "For help on these, call the funchelp function with the function name as the input." << "\n\n";
    return;
}

//adapted from https://stackoverflow.com/questions/60890441/
//Converts a python dictionary of string:array to a C++ map
map<string, vector<float> > convert_dict_to_map(py::dict dictionary){
    map<string, vector<float> > Result;
    for (pair<py::handle, py::handle> item : dictionary){
        auto key = item.first.cast<string>();
        auto value = item.second.cast<vector<float> >();
        Result[key] = value;
    }
    return Result;
}

//run FakeFlight with all desired inputs
//sample call:
//  FakeFlight(start_time, stop_time, model, file_dir, variable_list, 65., -65., 363., 450000., 350000.,
//             0.01, 2., 1, 0, 1, 5000, 1);
map<string, vector<float> > FakeFlight(float const start_time, float const stop_time, string const model, 
string const file_dir, vector<string> const variable_list, float const max_lat=65., float const min_lat=-65., 
float const lon_perorbit=363., float const max_height=450000., float const min_height=400000., float const p=0.01, 
float const n=2., int const plots=1, int const daily_plots=0, int const plot_close=1, int const plot_sampling=5000, 
int const trajplot_close=1)
{
    //Start python and import module from python
    py::scoped_interpreter guard{};  // S has to be imported once because interpreter restart isn't clean
    py::list path_list = py::module_::import("sys").attr("path");
    path_list.append("./Kamodo-master/kamodo/readers/");    
    py::module_ S = py::module_::import("SatelliteFlythrough");

    //run FakeFlight from python
    py::object results = S.attr("FakeFlight")(start_time, stop_time, model, file_dir, variable_list, 
                         "max_lat"_a=max_lat, "min_lat"_a=min_lat, "lon_perorbit"_a=lon_perorbit,
                         "max_height"_a=max_height, "min_height"_a=min_height, "p"_a=p, "n"_a=n, 
                         "plots"_a=plots, "daily_plots"_a=daily_plots, "plot_close"_a=plot_close,
                         "plot_sampling"_a=plot_sampling, "trajplot_close"_a=trajplot_close);
    auto Result = convert_dict_to_map(results);
    //py::print("The python interpreter is now closing.");
    return Result;
}

//run SampleTrajectory with all desired inputs
//sample call:
//  SampleTrajectory(start_time, stop_time, plot_dir, 65., -65., 363., 450000., 400000., 0.01, 2., 1, 1, 5000);
map<string, vector<float> > SampleTrajectory(float const start_time, float const stop_time,
string const plot_dir, float const max_lat=65., float const min_lat=-65., 
float const lon_perorbit=363., float const max_height=450000., float const min_height=400000., float const p=0.01, 
float const n=2., int const plots=1, int const plot_close=1, int const plot_sampling=5000)
{
    //Start python and import module from python
    py::scoped_interpreter guard{};  // S has to be imported once because interpreter restart isn't clean
    py::list path_list = py::module_::import("sys").attr("path");
    path_list.append("./Kamodo-master/kamodo/readers/");    
    py::module_ S = py::module_::import("SatelliteFlythrough");

    //run SampleTrajectory from python
    py::object results = S.attr("SampleTrajectory")(start_time, stop_time, plot_dir,
                         "max_lat"_a=max_lat, "min_lat"_a=min_lat, "lon_perorbit"_a=lon_perorbit,
                         "max_height"_a=max_height, "min_height"_a=min_height, "p"_a=p, "n"_a=n, 
                         "plots"_a=plots, "plot_close"_a=plot_close, "plot_sampling"_a=plot_sampling);
    auto Result = convert_dict_to_map(results);
    //py::print("The python interpreter is now closing.");
    return Result;
}

//run ModelFlythrough with all desired inputs
//sample call:
//  ModelFlythrough(model, file_dir, variable_list, sat_time, sat_height, sat_lat, sat_lon, 1, 0, 1, 5000);
map<string, vector<float> > ModelFlythrough(string const model, string const file_dir, vector<string> const variable_list,
vector<float> const sat_time, vector<float> const sat_height, vector<float> const sat_lat, vector<float> const sat_lon,
int const plots=1, int const daily_plots=0, int const plot_close=1, int const plot_sampling=5000)
{
    //Start python and import module from python
    py::scoped_interpreter guard{};  // S has to be imported once because interpreter restart isn't clean
    py::list path_list = py::module_::import("sys").attr("path");
    path_list.append("./Kamodo-master/kamodo/readers/");    
    py::module_ S = py::module_::import("SatelliteFlythrough");  
    //breaks here after calling SampleTrajectory because python memory not clean

    //run ModelFlythrough from python
    py::object results = S.attr("ModelFlythrough")(model, file_dir, variable_list, sat_time, sat_height, sat_lat, sat_lon,
                         "plots"_a=plots, "daily_plots"_a=daily_plots, "plot_close"_a=plot_close, "plot_sampling"_a=plot_sampling);
    py::print(results);
    auto Result = convert_dict_to_map(results);
    py::print("The python interpreter is now closing.");
    return Result;
}

//run ModelVariables
//sample call:
//  ModelVariables(model);
void ModelVariables(string const model="")
{
    //Start python and import module from python
    py::scoped_interpreter guard{};  // S has to be imported once because interpreter restart isn't clean
    py::list path_list = py::module_::import("sys").attr("path");
    path_list.append("./Kamodo-master/kamodo/readers/");    
    py::module_ S = py::module_::import("SatelliteFlythrough");

    //run ModelVariables from python
    py::object results = S.attr("ModelVariables")(model);
    //py::print("The python interpreter is now closing.");
    return;
}

//testing functions
int testFakeFlightCTIPe() {
    //Can only call one wrapped python function from above per build because python memory is not cleanly removed

    //FakeFlight test   GOOD
    //Initialize parameters and print
    float start_time=1426630000.0;
    float stop_time=1426791280.0;
    string model="CTIPe";
    string file_dir="C:/Users/rringuet/KamodoCXX_WinDev1/CTIPe/";
    vector<string> variable_list{"T_i","TEC"};
    cout<<start_time<<" "<<stop_time<<" "<<model<<" "<<file_dir<<" "<<variable_list[0]<<" "<<variable_list[1]<<"\n";

    //call FakeFlight, print the first three values of each key from C++ to prove it works
    auto results = FakeFlight(start_time, stop_time, model, file_dir, variable_list, 65., -65., 363., 450000., 350000.,
                   0.01, 2., 1, 0, 1, 5000, 1);
    map<string, vector<float> >::iterator it;
    for (it = results.begin(); it != results.end(); it++){
        cout << it->first    // string (key)
              << ':'
              << it->second[0] << " " << it->second[1] << " " << it->second[2]   // string's values 
              << endl;
    };
    return 0;
}

int testFakeFlightIRI() {
    //Can only call one wrapped python function from above per build because python memory is not cleanly removed

    //FakeFlight test   GOOD
    //Initialize parameters and print
    float start_time=1495915560.0;
    float stop_time=1496014100.0;
    string model="IRI";
    string file_dir="C:/Users/rringuet/KamodoCXX_WinDev1/IRI/";
    vector<string> variable_list{"T_i","TEC"};
    cout<<start_time<<" "<<stop_time<<" "<<model<<" "<<file_dir<<" "<<variable_list[0]<<" "<<variable_list[1]<<"\n";

    //call FakeFlight, print the first three values of each key from C++ to prove it works
    auto results = FakeFlight(start_time, stop_time, model, file_dir, variable_list, 65., -65., 363., 450000., 350000.,
                   0.01, 1., 1, 0, 1, 5000, 1);
    map<string, vector<float> >::iterator it;
    for (it = results.begin(); it != results.end(); it++){
        cout << it->first    // string (key)
              << ':'
              << it->second[0] << " " << it->second[1] << " " << it->second[2]   // string's values 
              << endl;
    };
    return 0;
}

int testSampleTrajectory(){
    //SampleTrajectory test   GOOD
    //Initialize parameters
    float start_time=1426660000.0;
    float stop_time=1426880700.0;
    string plot_dir="C:/Users/rringuet/KamodoCXX_WinDev1/CTIPe/";

    //call SampleTrajectory, print the first three values of each key from C++ to prove it works
    auto traj_results = SampleTrajectory(start_time, stop_time, plot_dir, 65., -65., 363., 450000., 400000., 0.01, 
                                    2., 1, 1, 5000);
    cout << "Printing a few elements from SampleTrajectory test" << endl;
    map<string, vector<float> >::iterator it;
    for (it = traj_results.begin(); it != traj_results.end(); it++){
        cout << it->first    // string (key)
              << ':'
              << it->second[0] << " " << it->second[1] << " " << it->second[2]   // string's values 
              << endl;
    }
    return 0;
}

int testModelFlythrough(){    
    //ModelFlythrough test (using a few points from SampleTrajectory test for easy testing)  GOOD
    //Initialize parameters
    string model="CTIPe";
    vector<string> variable_list{"T_i","TEC"};
    string file_dir="C:/Users/rringuet/KamodoCXX_WinDev1/CTIPe/";
    vector<float> sat_time{1426660000.,1426660002.,1426660004.,1426660006.};
    vector<float> sat_height{425000.,425058.,425116.,425170.};
    vector<float> sat_lat{65.,64.9998,64.9993,64.9988};
    vector<float> sat_lon={0.,0.134445,0.26889,0.39};

    //call ModelFlythrough, print the first three values of each key from C++ to prove it works
    auto results = ModelFlythrough(model, file_dir, variable_list, sat_time, sat_height, sat_lat, sat_lon, 
                                   1, 0, 1, 5000);
    cout << "Printing a few elements from ModelFlythrough test" << endl;
    map<string, vector<float> >::iterator it2;
    for (it2 = results.begin(); it2 != results.end(); it2++){
        cout << it2->first    // string (key)
              << ':'
              << it2->second[0] << " " << it2->second[1] << " " << it2->second[2]   // string's values 
              << endl;
    }
    return 0;
}

void testModelVariable1(){
    //ModelVariable test (version1: list the models) GOOD
    //Initialize parameters, then call ModelVariables
    string model="";
    ModelVariables(model);
    return;
}

void testModelVariable2(){
    //ModelVariable test (version2: list the variables for the CTIPe model)  GOOD
    //Initialize parameters, then call ModelVariables
    string model="CTIPe";
    ModelVariables(model);
    return;
}

/*void main(){
    //int test1 = testFakeFlight();  //GOOD
    //int test2 = testSampleTrajectory();  //GOOD
    //int test3 = testModelFlythrough();  //GOOD
    //testModelVariable1();  //GOOD
    //testModelVariable2();  //GOOD
    return;
}*/

