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

  std::mt19937 rng{std::random_device{}()};
  std::vector<ConstructionConfiguration> configs = {
      {seed_farthest_from_depot, eval_distance_only, better_distance},
      {seed_earliest_due_date, eval_distance_only, better_distance},
      {seed_earliest_due_date, eval_distance_with_slack,
       better_distance_then_tight},
  };

  for (int i = 0; i < 20; i++) {
    configs.emplace_back(seed_random(rng), eval_distance_only,
                         better_distance);
  }

  for (int i = 0; i < 20; i++) {
    configs.emplace_back(seed_random(rng), eval_distance_with_slack,
                         better_distance_then_tight);
  }

  std::vector<Solution> greedy_solutions = {};
  for (const auto& conf : configs) {
    Solution s =
        construct_single_solomon(nodes, distance_matrix, vehicle.capacity,
                                 conf.seed, conf.eval, conf.better);
    greedy_solutions.push_back(s);
  }

  std::optional<Solution> best;
  for (int i = 0; i < greedy_solutions.size(); i++) {
    Solution s = greedy_solutions[i];
    if (!best) {
      best = s;
    }

    if (s.routes.size() < best->routes.size() ||
        (s.routes.size() == best->routes.size() &&
         s.distance < best->distance)) {
      best = s;
    }

    std::println("Solution {} has routes.size() {} and distance {}", i,
                 s.routes.size(), s.distance);
  }

  std::println("Best solution found:");
  print_solution(*best);

  return 0;
}
