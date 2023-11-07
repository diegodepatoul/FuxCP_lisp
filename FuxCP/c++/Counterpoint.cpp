#include "headers/Counterpoint.hpp"
#include <iostream>
#include <exception>

/***********************************************************************************************************************
 *                                                                                                                     *
 *                                                                                                                     *
 *                                             Counterpoint class methods                                          *
 *                                                                                                                     *
 *                                                                                                                     *
 ***********************************************************************************************************************/

/**
 * Constructor
 * @param s the number of chords in the progression
 * @param cantus_firmus a pointer to a Tonality object
 * Returns a Counterpoint object
 */
// Counterpoint::Counterpoint(int s, Tonality *t, vector<int> chordDegs, vector<int> chordStas){
Counterpoint::Counterpoint (int s, vector<int> cantus_firmus, int species_, int* scale_, int* chromatic_scale_, int tone_pitch_cf_, int mode_param_, int* borrowed_scale_, int* off_scale_) {
    std::cout << "Entering Counterpoint class" << std::endl;
    size = s;
    cf = cantus_firmus;
    cp = IntVarArray(*this, cf.size(), 0, 127); // tonality->get_tonality_notes()
    species = species_;
    scale = scale_;
    chromatic_scale = chromatic_scale_;
    tone_pitch_cf = tone_pitch_cf_;
    mode_param = mode_param_;
    borrowed_scale = borrowed_scale_;
    off_scale = off_scale_;

    std::cout << "Potential values for variable: ";
    for (size_t i = 0; i < size; i++)
    {
        std::cout << cp[i] << " ";
    }
    std::cout << std::endl;

    for (int i = 0; i < size; ++i) {
        rel(*this, cp[i] == cf[i]);
    }

    std::cout << "Potential values for variable: ";
    for (size_t i = 0; i < size; i++)
    {
        std::cout << cp[i] << " ";
    }
    std::cout << std::endl;

    exampleCost = IntVar(*this, 1, 3);

    /**-----------------------------------------------------------------------------------------------------------------
    |                                                                                                                  |
    |                                                       Branching                                                  |
    |                                                                                                                  |
    -------------------------------------------------------------------------------------------------------------------*/

    // @todo make it smarter when it becomes necessary
    std::cout << "Branching:" << std::endl;
    branch(*this, cp, INT_VAR_DEGREE_MAX(), INT_VAL_MIN());
    std::cout << "Exiting Counterpoint" << std::endl;

}

Counterpoint::Counterpoint (int s, vector<int> cantus_firmus) {
    std::cout << "Entering Counterpoint class" << std::endl;
    cf = cantus_firmus;
    cp = IntVarArray(*this, cf.size(), 0, 127); // tonality->get_tonality_notes()
    size = s;
    std::cout << "Potential values for variable: ";
    for (size_t i = 0; i < size; i++)
    {
        std::cout << cp[i] << " ";
    }
    std::cout << std::endl;

    for (int i = 0; i < size; ++i) {
        rel(*this, cp[i] == cf[i]);
    }

    std::cout << "Potential values for variable: ";
    for (size_t i = 0; i < size; i++)
    {
        std::cout << cp[i] << " ";
    }
    std::cout << std::endl;

    exampleCost = IntVar(*this, 1, 3);

    /**-----------------------------------------------------------------------------------------------------------------
    |                                                                                                                  |
    |                                                       Branching                                                  |
    |                                                                                                                  |
    -------------------------------------------------------------------------------------------------------------------*/

    // @todo make it smarter when it becomes necessary
    std::cout << "Branching:" << std::endl;
    branch(*this, cp, INT_VAR_DEGREE_MAX(), INT_VAL_MIN());
    std::cout << "Exiting Counterpoint" << std::endl;

}

/**
 * Cost function for lexicographical minimization. The order is as follows:
 * 1. number of diminished chords with more than 3 notes
 * 2. number of chords with less than 4 note values
 * 3. number of fundamental state chords without doubled bass
 * 4. sum of melodic intervals minimizes the melodic movement between chords
 * @return the cost variables in order of importance
 */
IntVarArgs Counterpoint::cost() const {
    return {exampleCost};
    /*h
    // @todo maybe give the voices a priority + check the order depending on what is more important
    return {nOfDiminishedChordsWith4notes, nOfChordsWithLessThan4notes, nOfFundamentalStateChordsWithoutDoubledBass,
            sumOfMelodicIntervals, nOfCommonNotesInSoprano};*/
}

/**
 * Copy constructor
 * @param s an instance of the Counterpoint class
 * @return a copy of the given instance of the Counterpoint class
 */
Counterpoint::Counterpoint(Counterpoint& s): IntLexMinimizeSpace(s){
    cf = s.cf;
    cp = s.cp;
    size = s.size;
}

/**
 * Returns the size of the problem
 * @return an integer representing the size of the vars array
 */
int Counterpoint::get_size() const{
    return size;
}

/**
 * Copy method
 * @return a copy of the current instance of the Counterpoint class. Calls the copy constructor
 */
Space* Counterpoint::copy() {
    return new Counterpoint(*this);
}

/**
 * Returns the values taken by the variables vars in a solution
 * @return an array of integers representing the values of the variables in a solution
 */
int* Counterpoint::return_solution(){
    std::cout << "Potential values for variable: ";
    for (size_t i = 0; i < 3; i++)
    {
        std::cout << cp[i] << " ";
    }
    std::cout << std::endl;

    std::cout << "Converting the solution" << std::endl;
    int* solution = new int[size];
    for(int i = 0; i < size; i++){
        solution[i] = cp[i].val();
    }
    return solution;
}

/**
 * Constrain method for bab search
 * not needed for optimization problems as it is already implemented
 * @param _b a solution to the problem from which we wish to add a constraint for the next solutions
 */
//void Counterpoint::constrain(const Space& _b) {
//    const auto &b = dynamic_cast<const Counterpoint &>(_b);
//}

/**
 * Prints the solution in the console
 */
void Counterpoint::print_solution(){
    for(int i = 0; i < size; i++){
        cout << cp[i].val() << " ";
    }
    cout << endl;
}

/**
     * returns the parameters in a string
     * @return a string containing the parameters of the problem
     */
string Counterpoint::parameters(){
    //throw UnimplementedException();
    /*
    string message = "-----------------------------------------parameters-----------------------------------------\n";
    message += "Number of chords: " + std::to_string(size) + "\n";
    message += "Tonality: " + midi_to_letter(tonality->get_tonic()) + " " + mode_int_to_name(tonality->get_mode()) + "\n";
    message += "Chords: \n";
    for(int i = 0; i < size; i++){
        message += degreeNames[chordDegrees[i]] + " in " +
                   stateNames[chordStates[i]];
        if(i != size - 1)
            message += ",\n";
    }
    return message + ".\n";*/
}

/**
 * toString method
 * @todo make a toString method for IntVarArrays so the code here is cleaner
 * @return a string representation of the current instance of the Counterpoint class.
 * Right now, it returns a string "Counterpoint object. size = <size>"
 * If a variable is not assigned when this function is called, it writes <not assigned> instead of the value
 */
string Counterpoint::to_string(){
    string message;
    message += "********************************************************************************************\n";
    message += "*                                                                                          *\n";
    message += "*                                          Solution                                        *\n";
    message += "*                                                                                          *\n";
    message += "********************************************************************************************\n\n";
    /*
     * message += parameters();
     * */

    message += "\n-----------------------------------------variables------------------------------------------\n";
/*
    message += "BassTenorHarmonicIntervals = " + intVarArray_to_string(bassTenorHarmonicIntervals) + "\n";
    message += "TenorAltoHarmonicIntervals = " + intVarArray_to_string(tenorAltoHarmonicIntervals) + "\n";
    message += "AltoSopranoHarmonicIntervals = " + intVarArray_to_string(altoSopranoHarmonicIntervals) + "\n\n";

    message += "BassMelodicIntervals = " + intVarArray_to_string(bassMelodicIntervals) + "\n";
    message += "TenorMelodicIntervals = " + intVarArray_to_string(tenorMelodicIntervals) + "\n";
    message += "AltoMelodicIntervals = " + intVarArray_to_string(altoMelodicIntervals) + "\n";
    message += "SopranoMelodicIntervals = " + intVarArray_to_string(sopranoMelodicIntervals) + "\n\n";

    message += "absoluteBassMelodicIntervals = " + intVarArray_to_string(absoluteBassMelodicIntervals) + "\n";
    message += "absoluteTenorMelodicIntervals = " + intVarArray_to_string(absoluteTenorMelodicIntervals) + "\n";
    message += "absoluteAltoMelodicIntervals = " + intVarArray_to_string(absoluteAltoMelodicIntervals) + "\n";
    message += "absoluteSopranoMelodicIntervals = " + intVarArray_to_string(absoluteSopranoMelodicIntervals) + "\n\n";
*/
    message += "🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵\n\n";
    message += "Counterpoint = " + intVarArray_to_string(cp) + "\n\n";
    message += "🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵🎵\n\n";
/*

    message += "-------------------------------cost-related auxiliary arrays------------------------------\n";

    message += "nDifferentValuesInDiminishedChord = " + intVarArray_to_string(nDifferentValuesInDiminishedChord) + "\n";
    message += "nDifferentValuesInAllChords = " + intVarArray_to_string(nDifferentValuesAllChords) + "\n";
    message += "nOccurrencesBassInFundamentalState = " + intVarArray_to_string(nOccurrencesBassInFundamentalState) + "\n\n";
    message += "nCommonNotesInSoprano = " + intVarArray_to_string(commonNotesInSoprano) + "\n\n";

    message += "------------------------------------cost variables----------------------------------------\n";

    message += "nOfDiminishedChordsWith4notes = " + intVar_to_string(nOfDiminishedChordsWith4notes) + "\n";
    message += "nOfChordsWithLessThan4notes = " + intVar_to_string(nOfChordsWithLessThan4notes) + "\n";
    message += "nOfFundamentalStateChordsWithoutDoubledBass = " +
            intVar_to_string(nOfFundamentalStateChordsWithoutDoubledBass) + "\n";
    message += "nOfCommonNotesInSoprano = " + intVar_to_string(nOfCommonNotesInSoprano) + "\n";
    message += "sumOfMelodicIntervals = " + intVar_to_string(sumOfMelodicIntervals) + "\n\n";
*/
    return message;
}