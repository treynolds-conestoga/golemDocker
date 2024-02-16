# Create docekr files
dockerize <- function(ip = '<IP ADDRESS>', port = "<PORT>", check = TRUE) {
  message("Creating Dockerfiles...")

  # Load all and check
  devtools::load_all()

  # Check app
  if (check) {
    outcome <- devtools::check()
  }

  # Guard: Errors found in app
  if (length(outcome$errors) > 0) {
    print(outcome)
  }

  # Create Docekr files
  else {
    golem::add_dockerfile_with_renv(host = ip, port = port, output_dir = './deploy', open = FALSE)
    message("App dockerized...")
    message("Please change the ip and port in the following file: DockerFile")
  }
}

# Create docker images from the dockerfiles
create_docker_image <- function(name = tolower(golem::get_golem_name())) {
  # Guard: If dockerfile not created, stop
  if (!dir.exists('deploy')) {
    stop("Error: Please run the dockerize function to containerize the shiny app")
  }

  # Create Base Image
  execute_cmd_commands(c(
    "cd deploy",
    glue::glue("docker build -f Dockerfile_base --progress=plain -t {name}_base .")))

  # Create Actual Image
  execute_cmd_commands(c(
    "cd deploy",
    glue::glue("docker build -f Dockerfile --progress=plain -t {name}:latest .")))
}

# Runs the created docker image
run_docker_image <- function(port, name = tolower(golem::get_golem_name())) {
  # Gurad: If dockerfile not created, stop
  if (!dir.exists('deploy')) {
    stop("Error: Please run the dockerize function to containerize the shiny app")
  }

  # Run docker image
  execute_cmd_commands(c(
    "cd deploy",
    glue::glue("docker run -p {port}:{port} {name}:latest")))
}

# Verbose function for execute_cmd_commands function
.execute_cmd_commands__verbose <- function(commands) {
  message("Executed commands:")

  # Print Commands
  commands %>%
    purrr::map2_chr(
      1:length(.),
      .f = function(cmd, i, ...) {
        glue::glue(" => {i}. '{cmd}'")
      }
    ) %>%

    {
      for (cmd in .) {
        message(cmd)
      }
    }
}

# Executes cmd commands
execute_cmd_commands <- function(commands, verbose = FALSE) {
  if (verbose) {
    .execute_cmd_commands__verbose(commands)
  }
  commands %>%
    paste(collapse = " && ") %>%
    shell()
}

# Retrieves IP Address
get_ip <- function(select_wsl = FALSE) {
  config_info <- system("ipconfig", intern=TRUE)

  ip_addresses <- config_info[grep("IPv4", config_info)] %>%
    {
      gsub(".*? ([[:digit:]])", "\\1", .)
    }

  # Return ip
  if (select_wsl) {
    ip_addresses[2]
  } else {
    ip_addresses[1]
  }
}



