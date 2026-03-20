# How to Install and Configure TouchOSC

May 28, 2025

## Introduction

TouchOSC is a modular control surface toolkit for designing and building custom controllers.

This guide explains how to install and configure TouchOSC for use with the smARTsynth environment.

## Downloading and Installing TouchOSC
1.	Visit the official website: https://hexler.net/touchosc
2.	Download the version for your operating system:
3.	Install the application following your system’s standard installation process.

## Setting Up OSC Connections
1.	Launch TouchOSC
2.	Open the Connections configuration (chain link icon)
3.	Go to the OSC tab
4.	Enable Connection 1
5.	Click Browse and select the target host/device
6.	Verify:
-	Host is correct
- Send Port is correctly set

## Setting Up MIDI Connections
1.	Download TouchOSC Bridge: https://hexler.net/touchosc
2.	Launch TouchOSC Bridge on your computer
3.	In TouchOSC:
-	Open Connections
-	Go to the Bridge tab
-	Enable Connection 1
-	Select your computer using Browse
4.	Go to the MIDI tab:
-	Enable Connection 1
-	Set:
-	Send Port → Bridge 1
-	Receive Port → Bridge 1

## Testing the Setup
1.	Switch to Control Surface Mode
-	Press the ▶ button
-	Or use Ctrl/Cmd + E
2.	Interact with the interface:
-	Move faders
-	Press buttons
3.	Confirm that your connected software/device responds correctly

## Troubleshooting Tips
	•	Ensure all devices are on the same network
	•	On Windows, temporarily disable firewall for testing
	•	Check that required ports are open
	•	Make sure no other applications are blocking connections

Official manual: https://hexler.net/touchosc/manual
