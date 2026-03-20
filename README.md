# Playing the Explorateur

The Explorateur is a modular organ with full dynamic wind control, see <https://orchestrionix.com/project/yves-rechsteiner-lexplorateur/>. 
In early 2026, the Explorateur was explored by various artists. Since there is no standardized software interface available yet, each artist sketched their
own tools to control the instrument from a computer. This repository collects the tools that were used between the 22nd of February and the 8th of March 2026.

## Mauricio Silva Orendain

One approach to interconnecting different hardware and software devices in a flexible way for interaction with L’Explorateur was to use Ableton Live together with TouchOSC. This system, which I call smARTsynth, enables the intuitive assignment of devices to specific ranks of the organ directly from a tablet interface (TouchOSC), as well as the manipulation of all meta-parameters of the smARTvalve (such as vibrato, speech, wind headroom, etc.) on a per-rank basis. In addition, it allows for the implementation of internal MIDI processing (FXs) using Ableton Live devices. It is also possible to compose self-playing pieces directly within Ableton through MIDI clips and automation, as well as to record any live interaction as MIDI data.

Note: Ableton License required. 

### smARTsynth

smARTsynth is a software environment designed to control L’Explorateur using multiple hardware devices simultaneously while providing access to all wind parameters available through the smARTvalve system.

The environment consists of:

-	an Ableton Live session containing custom-developed control devices
-	a TouchOSC layout mapped to the Ableton session for an intuitive control interface

An Ableton Live license is required to run the smARTsynth environment. In the future, a similar environment is planned using Reaper, with the goal of making the system fully open-source and freely accessible.

#### Requirements

To understand the internal routing and the connection between Ableton Live and TouchOSC, a basic knowledge of Ableton is recommended. This will allow users to fully utilize the virtual console and modify the system if necessary.

Once the system is set up, however, interaction through TouchOSC is designed to be intuitive and user-friendly directly on the tablet.

#### Setup

1.	Download the necessary files from the Downloads folder.
2.	Follow the setup instructions provided in the documentation.

#### Support

For troubleshooting, questions, or suggestions, please contact:

m.silvaorendain@icloud.com

## Johannes Keller

## Ron Katzman

## Ruedi Tobler

