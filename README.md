# Haskell Social Network Simulation

This project simulates a social network using Haskell. It creates users who send messages to each other. The messages are displayed on a web server running on localhost.

## Prerequisites

You will need to have Stack installed on your system to build and run this project. Stack is a cross-platform program for developing Haskell projects.

## Getting Started

Follow these steps to run the project:

1. Open your terminal.

2. Navigate to the project directory using `cd path/to/project`.

3. Build the project with `stack build`.

4. Run the project with `stack run`.

5. Open a web browser and go to `http://localhost:3000` to view the messages.

6. To stop the program, press `Ctrl+C` (or `Command+C` on macOS) in the terminal.

## Permissions

Ensure that port 3000 is allowed through your firewall settings. The steps to do this vary based on your operating system and configuration.

## Design Decisions

The `User` and `Message` types are modeled to represent a user and a message in the social network simulation. The `User` type has a `username` and a `mailbox`, which is an `MVar` containing a list of `Message` objects. The `Message` type represents a message sent from one user to another. It contains the `sender`, `receiver`, `content`, and `timestamp`.

The decision to use `MVar` for the mailbox was made to ensure thread safety. Since multiple threads could potentially access and modify a user's mailbox at the same time, `MVar` provides a way to ensure that only one thread can access it at a time.

## Web Server

The `startWebServer` function starts a web server that listens on port 3000. When a request is received, the server gets the current list of users and reads each user's mailbox. These messages are then passed to the `generateHtml` function along with the list of users to generate an HTML document that displays the messages for each user. The generated HTML is then sent as the response to the request.

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

## License

[MIT](https://choosealicense.com/licenses/mit/)