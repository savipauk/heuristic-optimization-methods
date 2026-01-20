#include <algorithm>
#include <cassert>
#include <filesystem>
#include <fstream>
#include <functional>
#include <optional>
#include <print>
#include <random>
#include <sstream>
#include <string>
#include <vector>

using DistanceMatrix = std::vector<std::vector<double>>;
using NodeList = std::vector<int>;

struct VehicleInfo {
  int count{};
  int capacity{};
};

struct Customer {
  int id{};
  int x{};
  int y{};
  int demand{};
  int ready_time{};
  int due_date{};
  int service_time{};
};

struct Route {
  NodeList nodes{};
  std::vector<int> start_times{};
  double distance{};
};

struct Solution {
  std::vector<Route> routes{};
  double distance{};
};

struct Cost {
  size_t vehicle_count{};
  double distance{};

  bool operator<(const Cost& other) const {
    if (vehicle_count != other.vehicle_count) {
      return vehicle_count < other.vehicle_count;
    }
    return distance < other.distance;
  }
};

struct Insertion {
  int customer{};
  size_t position{};
  double distance_delta{};
  int due_date_delta{};
  bool feasible{};
};

struct Individual {
  std::vector<int> permutation;
  Solution solution;
  Cost cost;
};

using SeedFunction = std::function<int(const std::vector<Customer>& nodes,
                                       const DistanceMatrix& distance_matrix,
                                       const NodeList& unroutred)>;
using InsertionEvaluationFunction = std::function<Insertion(
    int customer, const Route& route, size_t pos,
    const std::vector<Customer>& nodes, const DistanceMatrix& distance_matrix,
    int vehicle_capacity)>;

using InsertionComparison =
    std::function<bool(const Insertion& a, const Insertion& b)>;

struct ConstructionConfiguration {
  SeedFunction seed;
  InsertionEvaluationFunction eval;
  InsertionComparison better;
};

double dist(const Customer& a, const Customer& b) {
  // Euclidean distance
  return std::hypot(a.x - b.x, a.y - b.y);
}

Cost cost(const Solution& s) {
  return {s.routes.size(), s.distance};
}

bool is_route_feasible(const NodeList& route,
                       const std::vector<Customer>& nodes,
                       const DistanceMatrix& distance_matrix,
                       int vehicle_capacity) {
  int current_time = 0;
  int current_load = 0;

  for (std::size_t i = 1; i < route.size(); ++i) {
    int from = route[i - 1];
    int to = route[i];

    const auto& prev = nodes[from];
    const auto& next = nodes[to];

    current_load += next.demand;
    if (current_load > vehicle_capacity) {
      return false;
    }

    int travel = (int)(std::ceil(distance_matrix[from][to]));
    int arrival = current_time + prev.service_time + travel;
    current_time = std::max(arrival, next.ready_time);

    if (current_time > next.due_date && to != 0) {
      return false;
    }
  }

  return true;
}

int compute_start_time(int prev_start, const Customer& prev,
                       const Customer& next, double distance) {
  int travel = (int)std::ceil(distance);
  int arrival = prev_start + prev.service_time + travel;
  return std::max(arrival, next.ready_time);
}

SeedFunction seed_farthest_from_depot =
    [](const std::vector<Customer>& nodes,
       const DistanceMatrix& distance_matrix, const NodeList& unrouted) {
      int seed = unrouted.front();
      for (int n : unrouted) {
        if (distance_matrix[0][n] > distance_matrix[0][seed]) seed = n;
      }
      return seed;
    };

SeedFunction seed_earliest_due_date = [](const std::vector<Customer>& nodes,
                                         const DistanceMatrix& distance_matrix,
                                         const NodeList& unrouted) {
  int seed = unrouted.front();
  for (int n : unrouted) {
    if (nodes[n].due_date < nodes[seed].due_date) seed = n;
  }
  return seed;
};

SeedFunction seed_first = [](const std::vector<Customer>& nodes,
                             const DistanceMatrix& distance_matrix,
                             const NodeList& unrouted) {
  int seed = unrouted.front();
  return seed;
};

SeedFunction seed_random(std::mt19937& rng) {
  return [&rng](const std::vector<Customer>& nodes, const DistanceMatrix&,
                const NodeList& unrouted) {
    std::uniform_int_distribution<size_t> d(0, unrouted.size() - 1);
    return unrouted[d(rng)];
  };
}

InsertionEvaluationFunction eval_distance_only =
    [](int customer, const Route& route, std::size_t pos,
       const std::vector<Customer>& nodes,
       const DistanceMatrix& distance_matrix, int vehicle_capacity) {
      Insertion ins{customer, pos, 0.0, 0, false};

      NodeList tmp = route.nodes;
      tmp.insert(tmp.begin() + pos, customer);

      if (!is_route_feasible(tmp, nodes, distance_matrix, vehicle_capacity)) {
        return ins;
      }

      int i = route.nodes[pos - 1];
      int j = route.nodes[pos];

      double delta = distance_matrix[i][customer] +
                     distance_matrix[customer][j] - distance_matrix[i][j];

      ins.distance_delta = delta;
      ins.feasible = true;

      return ins;
    };

InsertionEvaluationFunction eval_distance_with_slack =
    [](int customer, const Route& route, size_t pos,
       const std::vector<Customer>& nodes,
       const DistanceMatrix& distance_matrix, int vehicle_capacity) {
      Insertion ins{customer, pos, 0.0, 0, false};

      int i = route.nodes[pos - 1];
      int j = route.nodes[pos];

      ins.distance_delta = distance_matrix[i][customer] +
                           distance_matrix[customer][j] - distance_matrix[i][j];

      // load check
      NodeList tmp = route.nodes;
      tmp.insert(tmp.begin() + ins.position, ins.customer);
      int current_load = 0;
      for (std::size_t i = 1; i < tmp.size(); ++i) {
        int to = tmp[i];

        const auto& next = nodes[to];

        current_load += next.demand;
        if (current_load > vehicle_capacity) {
          return ins;  // infeasible
        }
      }

      // i is the inserted node
      int start_i = route.start_times[pos - 1];

      int start_customer = compute_start_time(
          start_i, nodes[i], nodes[customer], distance_matrix[i][customer]);

      if (start_customer > nodes[customer].due_date) {
        return ins;  // infeasible
      }

      // slack so far
      int min_slack = nodes[customer].due_date - start_customer;

      // start of the successor
      int new_start_j =
          compute_start_time(start_customer, nodes[customer], nodes[j],
                             distance_matrix[customer][j]);

      // if next node is depot or we miss the due date infeasible
      if (j != 0 && new_start_j > nodes[j].due_date) {
        return ins;
      }

      // include successor slack
      if (j != 0) {
        min_slack = std::min(min_slack, nodes[j].due_date - new_start_j);
      }

      // if the successor start time doesnt increase, the rest of the route
      // stays identical because it depends on prev_start
      if (new_start_j <= route.start_times[pos]) {
        ins.feasible = true;
        ins.due_date_delta =
            min_slack;  // higher is better because we want to visit as many
                        // nodes as possible with a vehicle
        return ins;
      }

      // otherwise update from the successor onwards
      int prev_start = new_start_j;
      int prev_node = j;

      for (size_t k = pos + 1; k < route.nodes.size(); ++k) {
        int to = route.nodes[k];

        int new_start_to =
            compute_start_time(prev_start, nodes[prev_node], nodes[to],
                               distance_matrix[prev_node][to]);

        if (to != 0 && new_start_to > nodes[to].due_date) {
          return ins;  // infeasible
        }

        if (to != 0)
          min_slack = std::min(min_slack, nodes[to].due_date - new_start_to);

        // if this nodes start time isnt pushed back, then everything else stays
        // the same
        if (new_start_to <= route.start_times[k]) {
          ins.feasible = true;
          ins.due_date_delta = min_slack;
          return ins;
        }

        prev_start = new_start_to;
        prev_node = to;
      }

      ins.feasible = true;
      ins.due_date_delta = min_slack;
      return ins;
    };

InsertionComparison better_distance = [](const Insertion& a,
                                         const Insertion& b) {
  return a.distance_delta < b.distance_delta;
};

InsertionComparison better_distance_then_tight = [](const Insertion& a,
                                                    const Insertion& b) {
  constexpr double eps = 1e-6;
  if (a.distance_delta < b.distance_delta - eps) return true;
  if (a.distance_delta > b.distance_delta + eps) return false;
  return a.due_date_delta < b.due_date_delta;
};

Route construct_route_l1(const std::vector<Customer>& nodes,
                         const DistanceMatrix& distance_matrix,
                         NodeList& unrouted, int vehicle_capacity,
                         const SeedFunction& pick_seed,
                         const InsertionEvaluationFunction& eval,
                         const InsertionComparison& better) {
  Route route;

  int seed = pick_seed(nodes, distance_matrix, unrouted);
  std::erase_if(unrouted, [seed](int c) { return c == seed; });

  route.nodes = {0, seed, 0};
  route.start_times = {0};

  for (int i = 1; i < route.nodes.size(); ++i) {
    int from = route.nodes[i - 1];
    int to = route.nodes[i];

    int st = compute_start_time(route.start_times.back(), nodes[from],
                                nodes[to], distance_matrix[from][to]);
    route.start_times.push_back(st);
  }

  // Ends when no more nodes can be added to the route
  while (!unrouted.empty()) {
    std::optional<Insertion> best;

    for (int u : unrouted) {
      for (std::size_t pos = 1; pos < route.nodes.size(); ++pos) {
        Insertion ins =
            eval(u, route, pos, nodes, distance_matrix, vehicle_capacity);

        if (!ins.feasible) {
          continue;
        }

        if (!best || better(ins, *best)) {
          best = ins;
        }
      }
    }

    // No feasible nodes to insert
    if (!best) {
      break;
    }

    // best->position is where in the route it will be inserted
    route.nodes.insert(route.nodes.begin() + best->position, best->customer);
    assert(is_route_feasible(route.nodes, nodes, distance_matrix,
                             vehicle_capacity));

    route.start_times.clear();
    route.start_times.push_back(0);
    for (int i = 1; i < route.nodes.size(); ++i) {
      int from = route.nodes[i - 1];
      int to = route.nodes[i];

      int st = compute_start_time(route.start_times.back(), nodes[from],
                                  nodes[to], distance_matrix[from][to]);

      route.start_times.push_back(st);
    }

    std::erase_if(unrouted, [&](const auto& c) { return c == best->customer; });
  }

  route.distance = 0.0;
  for (size_t i = 1; i < route.nodes.size(); ++i) {
    route.distance += distance_matrix[route.nodes[i - 1]][route.nodes[i]];
  }

  return route;
}

Solution construct_single_solomon(const std::vector<Customer>& nodes,
                                  const DistanceMatrix& distance_matrix,
                                  int vehicle_capacity, SeedFunction pick_seed,
                                  InsertionEvaluationFunction eval,
                                  InsertionComparison better) {
  Solution sol;

  NodeList unrouted;
  unrouted.reserve(nodes.size() - 1);
  for (std::size_t i = 1; i < nodes.size(); ++i) {
    unrouted.push_back(nodes[i].id);
  }

  while (!unrouted.empty()) {
    Route route = construct_route_l1(nodes, distance_matrix, unrouted,
                                     vehicle_capacity, pick_seed, eval, better);
    if (!is_route_feasible(route.nodes, nodes, distance_matrix,
                           vehicle_capacity)) {
      std::println("constructed an infeasible route");
    }

    sol.routes.push_back(route);
    sol.distance += route.distance;
  }

  return sol;
}

NodeList flatten_to_perm(const Solution& s,
                         const std::vector<Customer>& nodes) {
  NodeList perm;
  perm.reserve(nodes.size());
  for (auto& r : s.routes) {
    for (int v : r.nodes) {
      if (v != 0) perm.push_back(v);
    }
  }
  return perm;
}

static inline Cost INF_COST() {
  return {std::numeric_limits<size_t>::max() / 4, 1e100};
}

// turns a giant tour into a routed solution
Solution split_vrptw(const NodeList& perm, const std::vector<Customer>& nodes,
                     const DistanceMatrix& distance_matrix,
                     int vehicle_capacity) {
  int m = (int)perm.size();

  // dp[k] = best cost to serve the first k customers of perm
  std::vector<Cost> dp(m + 1, INF_COST());
  // pred[j] = how many customers were already served before starting the last
  // vehicle
  std::vector<int> pred(m + 1, -1);

  dp[0] = {0u, 0.0};
  pred[0] = 0;

  for (int i = 0; i < m; ++i) {
    if (pred[i] == -1) {
      continue;
    }

    int load = 0;
    int time = 0;  // start time at prev
    int prev = 0;  // depot
    double prefix_dist = 0.0;

    for (int j = i; j < m; ++j) {
      int customer = perm[j];

      // capacity
      load += nodes[customer].demand;
      if (load > vehicle_capacity) {
        break;
      }

      // travel + time windows
      int travel = (int)std::ceil(distance_matrix[prev][customer]);
      int arrival = time + nodes[prev].service_time + travel;
      time = std::max(arrival, nodes[customer].ready_time);

      if (time > nodes[customer].due_date) {
        break;
      }

      // distance accumulation up to customer
      prefix_dist += distance_matrix[prev][customer];
      prev = customer;

      // route ends at customer then returns to depot
      double route_dist = prefix_dist + distance_matrix[prev][0];

      Cost cand = {dp[i].vehicle_count + 1, dp[i].distance + route_dist};
      int nextPos = j + 1;

      if (cand < dp[nextPos]) {
        dp[nextPos] = cand;
        pred[nextPos] = i;
      }
    }
  }

  // no feasible split
  // perm is infeasible as a tour
  // can happen after crossover/mutation
  if (pred[m] == -1) {
    return Solution{};  // signal infeasible decoding
  }

  // reconstruct segments [i, j)
  // route is from i to j, [i, j)
  // nodes 0...i-1 were served by earlier routes
  std::vector<std::pair<int, int>> segs;
  for (int j = m; j > 0;) {
    int i = pred[j];
    segs.push_back({i, j});
    j = i;
  }
  std::reverse(segs.begin(), segs.end());

  Solution sol;
  sol.distance = 0.0;

  for (auto [i, j] : segs) {
    Route r;
    r.nodes.reserve((j - i) + 2);
    r.start_times.clear();

    r.nodes.push_back(0);
    r.start_times.push_back(0);

    int time = 0;
    int prev = 0;
    int load = 0;

    // build route in order with start times
    for (int k = i; k < j; ++k) {
      int customer = perm[k];
      load += nodes[customer].demand;

      int travel = (int)std::ceil(distance_matrix[prev][customer]);
      int arrival = time + nodes[prev].service_time + travel;
      int start = std::max(arrival, nodes[customer].ready_time);

      // these should always hold because dp checked
      if (load > vehicle_capacity || start > nodes[customer].due_date) {
        return Solution{};
      }

      r.nodes.push_back(customer);
      r.start_times.push_back(start);

      time = start;
      prev = customer;
    }

    int travel = (int)std::ceil(distance_matrix[prev][0]);
    int arrival = time + nodes[prev].service_time + travel;
    int depot_start = std::max(arrival, nodes[0].ready_time);

    r.nodes.push_back(0);
    r.start_times.push_back(depot_start);

    // route distance
    r.distance = 0.0;
    for (size_t t = 1; t < r.nodes.size(); ++t) {
      r.distance += distance_matrix[r.nodes[t - 1]][r.nodes[t]];
    }

    sol.routes.push_back(std::move(r));
    sol.distance += sol.routes.back().distance;
  }

  return sol;
}

NodeList ox_crossover(const NodeList& A, const NodeList& B, int node_count,
                      std::mt19937& rng) {
  const int m = (int)A.size();
  std::uniform_int_distribution<int> distPos(0, m - 1);
  int l = distPos(rng);
  int r = distPos(rng);
  if (l > r) std::swap(l, r);
  // if l == r it would be empt
  if (l == r) {
    r = (l + 1) % m;
    if (l > r) std::swap(l, r);
  }

  NodeList child(m, -1);

  std::vector<bool> used(node_count, false);

  for (int i = l; i <= r; ++i) {
    child[i] = A[i];
    used[A[i]] = 1;
  }

  int write = (r + 1) % m;
  int read = (r + 1) % m;

  int filled = (r - l + 1);
  while (filled < m) {
    int candidate = B[read];
    if (candidate >= 0 && !used[candidate]) {
      // find next empty slot
      while (child[write] != -1) {
        write = (write + 1) % m;
      }
      child[write] = candidate;
      used[candidate] = true;
      ++filled;
    }
    read = (read + 1) % m;
  }

  return child;
}

size_t select_parent_index_binary_tournament(
    const std::vector<Individual>& population, std::mt19937& rng) {
  assert(!population.empty());
  if (population.size() == 1) return 0;

  std::uniform_int_distribution<size_t> d(0, population.size() - 1);

  size_t a = d(rng);
  size_t b = d(rng);
  while (b == a) b = d(rng);

  // pick the better
  if (population[b].cost < population[a].cost) {
    return b;
  }

  return a;
}

void mutate_perm(NodeList& perm, std::mt19937& rng) {
  std::uniform_int_distribution<size_t> d(0, perm.size() - 1);
  size_t a = d(rng);
  size_t b = d(rng);
  while (b == a) {
    b = d(rng);
  }
  std::swap(perm[a], perm[b]);
}

Individual run_genetic_algorithm_for_time(
    const std::vector<Customer>& nodes, const DistanceMatrix& distance_matrix,
    int vehicle_capacity, std::vector<Individual> initial_population,
    size_t population_size, size_t offspring_per_generation,
    std::chrono::seconds time_limit, size_t max_generations,
    std::mt19937& rng) {
  assert(population_size > 0);
  assert(offspring_per_generation > 0);
  assert(!initial_population.empty());

  // shouldn't happen but just in case
  if (initial_population.size() > population_size) {
    std::sort(initial_population.begin(), initial_population.end(),
              [](const Individual& a, const Individual& b) {
                return a.cost < b.cost;
              });
    initial_population.resize(population_size);
  }

  std::vector<Individual> population = std::move(initial_population);

  Individual best = population[0];
  for (auto& ind : population) {
    if (ind.cost < best.cost) best = ind;
  }

  auto start = std::chrono::steady_clock::now();
  auto end_time = start + time_limit;

  std::uniform_real_distribution<double> prob01(0.0, 1.0);
  double mutation_probability = 0.9;

  size_t generation = 0;

  while (true) {
    auto now = std::chrono::steady_clock::now();

    bool time_exceeded = (time_limit.count() > 0) && (now >= end_time);
    bool generation_exceeded =
        (max_generations > 0) && (generation >= max_generations);

    if (time_exceeded || generation_exceeded) {
      break;
    }

    double t = 0.0;
    if (time_limit.count() > 0) {
      t = std::chrono::duration<double>(now - start).count() /
          std::chrono::duration<double>(end_time - start).count();
      t = std::clamp(t, 0.0, 1.0);
    }

    // to have mutation_probability go from 0.9 to 0.1 as time goes on
    mutation_probability = 0.9 - 0.8 * t;

    std::vector<Individual> offspring;
    offspring.reserve(offspring_per_generation);

    for (size_t k = 0; k < offspring_per_generation; ++k) {
      size_t ia = select_parent_index_binary_tournament(population, rng);
      size_t ib = select_parent_index_binary_tournament(population, rng);

      while (ib == ia && population.size() > 1) {
        ib = select_parent_index_binary_tournament(population, rng);
      }

      const NodeList& A = population[ia].permutation;
      const NodeList& B = population[ib].permutation;

      NodeList child_permutation = ox_crossover(A, B, nodes.size(), rng);

      if (prob01(rng) < mutation_probability) {
        mutate_perm(child_permutation, rng);
      }

      Solution decoded = split_vrptw(child_permutation, nodes, distance_matrix,
                                     vehicle_capacity);
      if (decoded.routes.empty()) {
        // infeasible decoding can happen after crossover or mutation
        // skip this child
        continue;
      }

      Cost c = cost(decoded);
      population.push_back(Individual{child_permutation, decoded, c});
      Individual child{std::move(child_permutation), std::move(decoded), c};

      if (child.cost < best.cost) {
        best = child;
      }

      offspring.push_back(std::move(child));
    }

    // elitism
    population.insert(population.end(),
                      std::make_move_iterator(offspring.begin()),
                      std::make_move_iterator(offspring.end()));

    // makes sure to keep the best in the next iter
    if (population.size() > population_size) {
      auto nth = population.begin() + population_size;
      std::nth_element(population.begin(), nth, population.end(),
                       [](const Individual& a, const Individual& b) {
                         return a.cost < b.cost;
                       });
      population.resize(population_size);
    }

    std::sort(population.begin(), population.end(),
              [](const Individual& a, const Individual& b) {
                return a.cost < b.cost;
              });

    // update best in case population improved due to survivor selection
    if (!population.empty() && population[0].cost < best.cost) {
      best = population[0];
    }

    // std::println("gen {} best_pop: ({}, {:.2f}) best_ever: ({}, {:.2f})",
    //              generation, population[0].cost.vehicle_count,
    //              population[0].cost.distance, best.cost.vehicle_count,
    //              best.cost.distance);
    ++generation;
  }

  std::println("GA ran for {} generations", generation);

  return best;
}

void print_solution(const Solution& sol) {
  std::println("{}", sol.routes.size());

  for (std::size_t i = 0; i < sol.routes.size(); ++i) {
    const auto& route = sol.routes[i];

    std::print("{}: ", i + 1);

    for (std::size_t i = 0; i < route.nodes.size(); ++i) {
      std::print("{}({})", route.nodes[i], route.start_times[i]);

      if (i + 1 < route.nodes.size()) {
        std::print("->");
      }
    }

    std::println("");
  }

  std::println("{:.2f}", sol.distance);
}

int main(int argc, char** argv) {
  if (argc < 2) {
    std::println(stderr, "Usage: {} <input.txt>", argv[0]);
    return 1;
  }

  std::filesystem::path input_path{argv[1]};
  std::ifstream file(input_path);

  if (!file) {
    std::println(stderr, "Failed to open file: {}", input_path.string());
    return 1;
  }

  VehicleInfo vehicle;
  std::vector<Customer> nodes;

  enum class Section { NONE, VEHICLE, CUSTOMER };

  std::string line;
  Section section = Section::NONE;

  while (std::getline(file, line)) {
    if (line.empty()) {
      continue;
    }

    if (line.starts_with("VEHICLE")) {
      section = Section::VEHICLE;
      continue;
    }

    if (line.starts_with("CUSTOMER")) {
      section = Section::CUSTOMER;
      continue;
    }

    // Column headers
    if (line.starts_with("NUMBER") || line.starts_with("CUST")) {
      continue;
    }

    std::istringstream iss(line);

    switch (section) {
      case Section::VEHICLE: {
        iss >> vehicle.count >> vehicle.capacity;
        break;
      }
      case Section::CUSTOMER: {
        Customer c;
        if (iss >> c.id >> c.x >> c.y >> c.demand >> c.ready_time >>
            c.due_date >> c.service_time) {
          nodes.push_back(c);
        }
        break;
      }
      case Section::NONE:
      default:
        break;
    }
  }

  assert(vehicle.capacity > 0);
  assert(!nodes.empty());
  // std::println("Vehicles: {} (capacity {})", vehicle.count,
  // vehicle.capacity); std::println("Customers parsed: {}", nodes.size());

  const std::size_t n = nodes.size();
  DistanceMatrix distance_matrix(n, std::vector<double>(n));

  for (std::size_t i = 0; i < n; ++i) {
    for (std::size_t j = i; j < n; ++j) {
      double d = dist(nodes[i], nodes[j]);
      distance_matrix[i][j] = d;
      distance_matrix[j][i] = d;
    }
  }

  const size_t population_size = 200;
  const size_t offspring_per_generation = 100;

  std::mt19937 rng{std::random_device{}()};
  std::vector<ConstructionConfiguration> configs = {
      {seed_first, eval_distance_with_slack, better_distance_then_tight},
      {seed_first, eval_distance_only, better_distance},
      {seed_farthest_from_depot, eval_distance_only, better_distance},
      {seed_farthest_from_depot, eval_distance_with_slack,
       better_distance_then_tight},
      {seed_earliest_due_date, eval_distance_only, better_distance},
      {seed_earliest_due_date, eval_distance_with_slack,
       better_distance_then_tight},
  };

  for (int i = 0; i < population_size / 2 - 3; i++) {
    configs.emplace_back(seed_random(rng), eval_distance_only, better_distance);
  }

  for (int i = 0; i < population_size / 2 - 3; i++) {
    configs.emplace_back(seed_random(rng), eval_distance_with_slack,
                         better_distance_then_tight);
  }

  std::vector<Individual> population;
  population.reserve(population_size);

  auto greedy_start = std::chrono::steady_clock::now();

  size_t conf_i = 0;
  while (population.size() < population_size) {
    const auto& conf = configs[conf_i];
    conf_i = (conf_i + 1) % configs.size();

    Solution s =
        construct_single_solomon(nodes, distance_matrix, vehicle.capacity,
                                 conf.seed, conf.eval, conf.better);
    NodeList perm = flatten_to_perm(s, nodes);
    Solution decoded =
        split_vrptw(perm, nodes, distance_matrix, vehicle.capacity);
    // shouldn't happen but just in case
    if (decoded.routes.empty()) {
      continue;
    }
    Cost c = cost(decoded);
    population.push_back(Individual{std::move(perm), std::move(decoded), c});
  }

  std::optional<Individual> best_greedy;
  for (const auto& ind : population) {
    if (!best_greedy || ind.cost < best_greedy->cost) {
      best_greedy = ind;
    }
  }

  std::println("Best greedy solution found:");
  print_solution(best_greedy->solution);

  auto greedy_end = std::chrono::steady_clock::now();

  const std::chrono::duration<double> elapsed = greedy_end - greedy_start;
  std::println("Initial population / Greedy time: {:.2f} s", elapsed.count());

  int seconds = 60;
  auto time_limit = std::chrono::seconds(seconds);
  std::println("Running Genetic Algorithm for {} seconds", seconds);

  Individual best_ga = run_genetic_algorithm_for_time(
      nodes, distance_matrix, vehicle.capacity,
      population,  // initial population
      population_size, offspring_per_generation, time_limit,
      std::numeric_limits<size_t>::max(), rng);

  std::println("Best GA solution found:");
  print_solution(best_ga.solution);

  return 0;
}
