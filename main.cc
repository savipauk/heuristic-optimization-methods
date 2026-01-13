#include <filesystem>
#include <fstream>
#include <print>
#include <sstream>
#include <string>
#include <vector>

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

double dist(const Customer& a, const Customer& b) {
  // Euclidean distance
  return std::hypot(a.x - b.x, a.y - b.y);
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
  std::vector<Customer> customers;

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
        iss >> c.id >> c.x >> c.y >> c.demand >> c.ready_time >> c.due_date >>
            c.service_time;
        customers.push_back(c);
        break;
      }
      case Section::NONE:
      default:
        break;
    }
  }

  std::println("Vehicles: {} (capacity {})", vehicle.count, vehicle.capacity);
  std::println("Customers parsed: {}", customers.size());

  Customer depot = customers.front();
  customers.erase(customers.begin());

  return 0;
}
