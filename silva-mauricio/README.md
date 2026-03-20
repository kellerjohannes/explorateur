# smARTsynth Control Environment

for Ableton Live and TouchOSC
Mauricio Silva Orendain
March 16, 2026

## Overview

The smARTsynth is a software environment designed to control L’Explorateur via MIDI using the smARTvalve system. It consists of a virtual console in Ableton Live, controllable through TouchOSC.

It enables:

- Selection of hardware and software devices per rank
- Independent wind control per pipe
- Control of meta parameters
- Simultaneous control of multiple ranks
- Integration of MIDI processes via Ableton

⸻

## Wind Control

Wind pressure per pipe can be controlled via:

### CC40

- 64 = standard pressure
- 0–63 = under-pressure
- 65–127 = over-pressure

### Pitch Bend

- 0 = standard pressure

### MPE Pressure / Polyphonic Aftertouch
- Same behavior as CC40

⸻

## MIDI Structure

Each rank (pipe group) has:
- Its own MIDI channel
- Its own note range

This allows independent control of multiple ranks.

⸻

## Meta Parameters

The smARTvalve responds to additional MIDI CC parameters, including:
- Wind headroom
- Vibrato depth
- Vibrato speed
- Envelope parameters (in development)

All parameters are accessible via TouchOSC.

⸻

## smARTsynth Environment

A preconfigured MIDI system in Ableton Live including:
- Custom MIDI devices
- Rank routing system
- FX chains
- TouchOSC mapping

All mappings are preconfigured after installation.

⸻

## Presets and FX

### Presets

Store configurations of smARTvalve parameters:
- Wind
- Headroom
- Vibrato

(Do not include Ableton devices)

### FX

Include MIDI processing in Ableton:
- Arpeggiator
- Chord
- MIDI transformations
- Custom processes

⸻

## Ableton Devices

### smARTsynth
- Main control device
- Contains all meta parameters
- Includes Master Wind control

### smARTrack
- Routes MIDI to specific ranks
- Uses External Instrument devices

### smARTfx
- Combines devices into musical processes

### smARTdevice
- Assigns external controllers to ranks
- Features:
- Rank selection
- Device activation
- Transposition
- Integrated FX

⸻

## System Tools

### Keep On Message

Prevents automatic shutdown by sending periodic MIDI signals.

### Master Wind
- Controls global wind headroom
- Sends CC41 to all ranks
- Overrides individual settings

### Full Reset
- Sends CC120 to all ranks
- Resets notes and parameters

### Parameters Control Room
- Central control interface
- Mirrors system state via TouchOSC

⸻

## Setup
1.	Download required files
2.	Install Ableton Pack and TouchOSC layout
3.	Configure MIDI outputs
4.	Save configuration as preset

⸻

## Support

For questions or troubleshooting:
m.silvaorendain@icloud.com
