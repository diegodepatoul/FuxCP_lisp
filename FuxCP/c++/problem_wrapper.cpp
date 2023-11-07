#include "headers/problem_wrapper.hpp"

void* set_up_counterpoint(int size, int* cf, int species, int* scale, int* chromatic_scale, int tone_pitch_cf, int mode_param, int* borrowed_scale, int* off_scale) {
    std::cout << "Entered set_up_counterpoint function" << std::endl;
    std::cout << "Size is " << size << std::endl;
    vector<int> cantus_firmus(int_pointer_to_vector(cf, size));
    auto* pb = new Counterpoint(size, cantus_firmus, species, scale, chromatic_scale, tone_pitch_cf, mode_param, borrowed_scale, off_scale);
    return (void*) make_solver(pb, 1);
}

int* get_counterpoint(void* solver) {
    std::cout << "Entered get_counterpoint function" << std::endl;
    auto* pb = static_cast<Counterpoint*>(get_next_solution_space(static_cast<DFS<Counterpoint>*>(solver)));
    int* sol = pb->return_solution();
}
/**
 * Wraps the Counterpoint constructor.
 * @todo modify this to include any parameters your Counterpoint constructor requires
 * @param size an integer representing the size of the problem
 * @param key the key of the tonality
 * @param mode the mode of the tonality
 * @param chord_degrees an integer array representing the chord degrees
 * @param chord_states an integer array representing the chord states
 * @return A pointer to a Counterpoint object casted as a void*
 */
void* create_new_problem(int size, int* cf){
    // throw UnimplementedException();
    /// date and time for logs
    std::cout << "Entered create_new_problem function" << std::endl;
    // write_to_log_file(time().c_str());
    std::cout << "Size is " << size << std::endl;

/*
    Tonality *t;
    if(mode == MAJOR_MODE) /// major mode
        t = new MajorTonality(key % PERFECT_OCTAVE);
    else if(mode == MINOR_MODE) /// minor mode
        t = new MinorTonality(key % PERFECT_OCTAVE);
    // @todo add other modes here if needed
    else /// default = major
        t = new MajorTonality(key % PERFECT_OCTAVE);
    vector<int> degrees(int_pointer_to_vector(chord_degrees, size));
    vector<int> states(int_pointer_to_vector(chord_states, size));
*/
    vector<int> cantus_firmus(int_pointer_to_vector(cf, size));
    auto* pb = new Counterpoint(size, cantus_firmus);
    std::cout << "Exiting create_new_problem" << std::endl;
    return (void*) pb;
}

/**
 * returns the size of the problem
 * @param sp a void* pointer to a Counterpoint object
 * @return an integer representing the size of the problem
 */
int get_size(void* sp){
    return static_cast<Counterpoint*>(sp)->get_size();
}

/**
 * creates a search engine for Counterpoint objects
 * @param sp a void* pointer to a Counterpoint object
 * @return a void* cast of a Base<Counterpoint>* pointer
 */
void* create_solver(void* sp, int type){
    return (void*) make_solver(static_cast<Counterpoint*>(sp), type);
}

/**
 * returns the next solution space, it should be bound. If not, it will return NULL.
 * @param solver a void* pointer to a Base<Counterpoint>* pointer for the search engine of the problem
 * @return a void* cast of a Counterpoint* pointer
 */
void* return_next_solution_space(void* solver){
    std::cout << "Calling return_next_solution_space: " << std::endl;
    std::cout << "with solver = " << solver << std::endl;
    return (void*) get_next_solution_space(static_cast<DFS<Counterpoint>*>(solver));
}


/**
 * returns the best solution space, it should be bound. If not, it will return NULL.
 * @param solver a void* pointer to a BAB<Counterpoint> for the search engine of the problem
 * @return a void* cast of a Counterpoint* pointer
 */
void* return_best_solution_space(void* solver){
    Counterpoint *bestSol; // keep a pointer to the best solution
    while(Counterpoint *sol = get_next_solution_space(static_cast<BAB<Counterpoint>*>(solver))){
        bestSol = sol;
    }
    std::cout << "Best solution found: \n\n" << bestSol->to_string() << std::endl;
    write_to_log_file(bestSol->to_string().c_str());
    return (void*) bestSol;
}

/**
 * returns the values of the variables for a solution
 * @param sp a void* pointer to a Counterpoint object
 * @return an int* pointer representing the values of the variables
 */
int* return_solution(void* sp){
    auto* pb = static_cast<Counterpoint*>(sp);
    int* sol = pb->return_solution();
    return sol;
}
