using namespace std;
#include <iostream>
#include <string>

/* Possible functions from functions.cpp
void funcdir(); 

map<string, vector<float> > FakeFlight(float const start_time, float const stop_time, string const model, 
string const file_dir, vector<string> const variable_list, float const max_lat=65., float const min_lat=-65., 
float const lon_perorbit=363., float const max_height=450000., float const min_height=400000., float const p=0.01, 
float const n=2., int const plots=1, int const daily_plots=0, int const plot_close=1, int const plot_sampling=5000, 
int const trajplot_close=1);
//sample call:
//  FakeFlight(start_time, stop_time, model, file_dir, variable_list, 65., -65., 363., 450000., 350000.,
//             0.01, 2., 1, 0, 1, 5000, 1);

map<string, vector<float> > SampleTrajectory(float const start_time, float const stop_time,
string const plot_dir, float const max_lat=65., float const min_lat=-65., 
float const lon_perorbit=363., float const max_height=450000., float const min_height=400000., float const p=0.01, 
float const n=2., int const plots=1, int const plot_close=1, int const plot_sampling=5000);
//sample call:
//  SampleTrajectory(start_time, stop_time, plot_dir, 65., -65., 363., 450000., 400000., 0.01, 2., 1, 1, 5000);

map<string, vector<float> > ModelFlythrough(string const model, string const file_dir, vector<string> const variable_list,
vector<float> const sat_time, vector<float> const sat_height, vector<float> const sat_lat, vector<float> const sat_lon,
int const plots=1, int const daily_plots=0, int const plot_close=1, int const plot_sampling=5000);
//sample call:
//  ModelFlythrough(model, file_dir, variable_list, sat_time, sat_height, sat_lat, sat_lon, 1, 0, 1, 5000);

void ModelVariables(string const model="");
//sample call:
//  ModelVariables(model);

//The functions below are all testing functions. You can only import one per session.
//See the related code for the test examples below on how to run the functions above.
int testFakeFlightCTIPe()
int testFakeFlightIRI()
int testSampleTrajectory()
int testModelFlythrough()
void testModelVariable1()
void testModelVariable2()
*/

int testFakeFlightIRI();
void funcdir();

void main(){
    int test1 = testFakeFlightIRI();  //GOOD
    //int test2 = testSampleTrajectory();  //GOOD
    //int test3 = testModelFlythrough();  //GOOD
    //testModelVariable1();  //GOOD
    //testModelVariable2();  //GOOD
    funcdir();  //GOOD
    return;
}