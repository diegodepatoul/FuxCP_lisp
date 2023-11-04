#ifndef MYPROJECT_SOLVER_HPP
#define MYPROJECT_SOLVER_HPP

#include "counterpoint.hpp"
#include "Utilities.hpp"

using namespace Gecode;
using namespace std;

/***********************************************************************************************************************
 *                                                                                                                     *
 *                                                Search engine methods                                                *
 *                                                                                                                     *
 ***********************************************************************************************************************/

/**
 * This file contains all the functions responsible for creating and using search engines for the Counterpoint class.
 */

/**
 * Creates a search engine for the given problem
 * @param pb an instance of the Counterpoint class representing a given problem
 * @param type the type of search engine to create (see enumeration in headers/gecode_problem.hpp)
 * @return a search engine for the given problem
 */
Search::Base<Counterpoint>* make_solver(Counterpoint* pb, int type);

/**
 * Returns the next solution space for the problem
 * @param solver a solver for the problem
 * @return an instance of the Counterpoint class representing the next solution to the problem
 */
Counterpoint* get_next_solution_space(Search::Base<Counterpoint>* solver);

/**
 * Returns the best solution for the problem pb. It uses a branch and bound solver with lexico-minimization of the costs
 * @param pb an instance of a Counterpoint problem
 * @return the best solution to the problem
 */
const Counterpoint* find_best_solution(Counterpoint *pb);

/**
 * Returns the first maxNOfSols solutions for the problem pb using the solver solverType.
 * @param pb an instance of a Counterpoint problem
 * @param solverType the type of the solver to use from solver_types
 * @param maxNOfSols the maximum number of solutions we want to find (the default value is MAX_INT)
 * @return the first maxNOfSols solutions to the problem
 */
vector<const Counterpoint*> find_all_solutions(Counterpoint *pb, int solverType,
                                                   int maxNOfSols = std::numeric_limits<int>::max());

#endif
