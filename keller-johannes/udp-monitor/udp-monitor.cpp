#include <iostream>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <cstdlib>  // For exit()
#include <cstring>

#define PORT 8888
#define MAXLINE 1024

int main() {
  int sockfd;
  unsigned char buffer[MAXLINE];
  struct sockaddr_in servaddr, cliaddr;
  socklen_t len = sizeof(cliaddr);

  // Create UDP socket
  if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    perror("Socket creation failed");
    exit(1);
  }

  // Zero out the server address structure
  memset(&servaddr, 0, sizeof(servaddr));

  // Configure server address
  servaddr.sin_family = AF_INET;
  servaddr.sin_addr.s_addr = INADDR_ANY;  // Listen on all available interfaces
  servaddr.sin_port = htons(PORT);

  // Bind the socket to the specified port
  if (bind(sockfd, (const struct sockaddr *)&servaddr, sizeof(servaddr)) < 0) {
    perror("Bind failed");
    close(sockfd);
    exit(1);
  }

  std::cout << "UDP monitor listening on port " << PORT << "...\n";

  while (true) {
    int n = recvfrom(sockfd, buffer, MAXLINE, 0, (struct sockaddr *)&cliaddr, &len);
    if (n < 0) {
      perror("recvfrom failed");
      break;  // Exit loop on error
    }

    buffer[n] = '\0';  // Null-terminate received data (for safety if treating as string)

    std::cout << "Received " << n << " bytes from "
              << inet_ntoa(cliaddr.sin_addr) << ":" << ntohs(cliaddr.sin_port)
              << " -> ";

    // Print payload in hexadecimal
    for (int i = 0; i < n; i++) {
      std::printf("x%02x ", (unsigned char)buffer[i]);
    }

    std::cout << std::endl;

    std::cout << "Parsed message: ";

    std::printf("ID1 x%02x (%i), ID2 x%02x (%i)",
                (unsigned char)buffer[0], (uint8_t)buffer[0],
                (unsigned char)buffer[1], (uint8_t)buffer[1]);

    for (int i = 2; i < n; i += 2) {
      if (i + 1 >= n)
        break;

      uint16_t value = (buffer[i] << 8) | buffer[i + 1];

      std::printf(", x%04x (%i)", value, (uint16_t)value);
    }

    std::cout << std::endl;

  }

  // Cleanup
  close(sockfd);
  return 0;
}
