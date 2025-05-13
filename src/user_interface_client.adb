-- This package provides client component for a distributed Clock user
-- interface. It ptovides a display of the current status and allows
-- commands to be sent to the IOT_Clock programme

-- Author    : David Haley
-- Created   : 25/07/2019
-- Last Edit : 11/05/2025

-- 20250511 : Provide for cycling sweep mode.
-- 20220609 : Port to 64 bit native compiler, Driver_Types renamed to
-- TLC5940_Driver_Types. User shutdown removed.
-- 20220127 : Corrected display of AL_Test_Value.
-- 20220126 : Performance enhancements to reduce CPU load when servicing
-- multiple clients, faster response when processing volume ap and down
-- requests.
-- 20220124 : New concept for diagnostic display, including animated clock.
-- 20220121 : Restricting screen updates to improve Windows performance.
-- 20220120 : Volume_Test added;
-- 20220119 : Toggle_Chime added Process_Request made two speed dependent on
-- Diagnostic State. Hide cursor during screen updates;
-- 20220116 : Made generic and implementation of chiming control via user
-- interface.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams; use Ada.Streams;
with Interfaces; use Interfaces;
with GNAT.Sockets; use GNAT.Sockets;
with ANSI_Console; use ANSI_console;
with TLC5940_Driver_Types; use TLC5940_Driver_Types;
with LED_Declarations; use LED_Declarations;
with Shared_User_Interface; use Shared_User_Interface;

package body User_Interface_Client is

   Server_Address : Sock_Addr_Type := (Family => Family_Inet,
                                       Addr => Addresses (Get_Host_By_Name
                                         (Clock_Name), 1),
                                       Port => Request_Port);

   Client_Address : Sock_Addr_Type := (Family => Family_Inet,
                                       Addr => Any_Inet_Addr,
                                       Port => Response_Port);

   package Boolean_IO is new Ada.Text_IO.Enumeration_IO (Boolean);

   task Process_Requests is
      entry RX_Ready;
      entry Start;
      entry Quick_Response;
      entry Version_Mismatch;
      entry Finished;
   end Process_Requests;

   task Update_Screen is
      entry Finished;
   end Update_Screen;

   procedure Run_UI is

   begin -- Run_UI
      Process_Requests.Start;
      Process_Requests.Finished;
      Update_Screen.Finished;
   end Run_UI;

   task body Process_Requests is

      procedure Read_Request (Run_Process_Requests : in out Boolean;
                              Request_Record : in out Request_Records) is

         Command : Character;
         Command_Available : Boolean;

      begin -- Read_Request
         Request_Record.Request := Get_Status;
         Get_Immediate (Command, Command_Available);
         if Command_Available then
            case Command is
            when 'D' | 'd' =>
               Request_Record.Diagnostic_Toggle :=
                 not Request_Record.Diagnostic_Toggle;
            when 'C' | 'c' =>
               Request_Record.Request := Toggle_Chime;
            when '+' =>
               Request_Record.Request := Volume_Up;
            when '-' =>
               Request_Record.Request := Volume_Down;
            when 'T' | 't' =>
               Request_Record.Request := Volume_Test;
            when 'S' | 's' =>
               Request_Record.Request := Cycle_Sweep;
            when 'E' | 'e' =>
               Run_Process_Requests := False;
               Request_Record.Request := Exit_User_Interface;
            when others =>
               null;
            end case; -- Command
         end if; -- Command_Available
      end Read_Request ;

      TX_Socket : Socket_Type;
      Request_Record : Request_Records;
      TX_Buffer : Request_Buffers;
      for TX_Buffer'Address use Request_Record'Address;
      pragma Import (Ada, TX_Buffer);
      Update_Interval : constant Duration := 1.0;
      -- update at 1 Hz except when diagnostics are enabled;
      Diagnostic_Interval : constant Duration := 1.0 / 8.0;
      -- 8 Hz to match clock updates
      Run_Process_Requests : Boolean := True;
      Next_Time : Time;
      Last : Stream_Element_Offset;

   begin -- Process_Requests
      accept Start;
      Create_Socket (Tx_Socket, Family_Inet, Socket_Datagram); -- UDP socket
      accept RX_Ready;
      Next_Time := Clock; -- deliberatly random sychronisation;
      while Run_Process_Requests loop
         select
            accept Quick_Response;
         or
            accept Version_Mismatch  do
               Run_Process_Requests := False;
            end Version_Mismatch;
         or
            delay until Next_Time;
            if Request_Record.Diagnostic_Toggle then
               Next_Time := Next_Time + Diagnostic_Interval;
            else
               Next_Time := Next_Time + Update_interval;
            end if; -- Request_Record.Request = Get_Status
         end select;
         Read_Request (Run_Process_Requests, Request_Record);
         Send_Socket (TX_Socket, TX_Buffer, Last, Server_Address);
      end loop; -- Run_Process_Commands
      Close_Socket (Tx_Socket);
      accept Finished;
   end Process_Requests;

   task body Update_Screen is

      RX_Socket : Socket_Type;
      Run_Screen_Update : Boolean := True;

      procedure Receive_Response (Status : in Status_Records;
                                  Previous_Status : in Status_Records) is

         procedure Animate_Clock (Origin_X : in X_Pos;
                                  Origin_Y : in Y_Pos;
                                  Status, Previous_Status : in Status_Records;
                                  Full_Update : in Boolean) is

            Passive : constant Character := ' ';

            type LED_Datum is record
               X : X_Pos;
               Y : Y_Pos;
               Active : Character;
            end record; -- LED_Datum

            type LED_Data_Arrays is array (LED_Drivers, LED_Channels) of
              LED_Datum;

            LED_Data : constant LED_Data_Arrays :=
              (
               Sweep_00_14 =>
                 (
                  16#0# => (13, 00, '-'), -- 00
                  16#1# => (14, 00, '-'), -- 01
                  16#2# => (15, 00, '-'), -- 02
                  16#3# => (16, 00, '-'), -- 03
                  16#4# => (17, 00, '-'), -- 04
                  16#5# => (19, 01, '\'), -- 05
                  16#6# => (20, 02, '\'), -- 06
                  16#7# => (21, 03, '\'), -- 07
                  16#8# => (22, 04, '\'), -- 08
                  16#9# => (23, 05, '\'), -- 09
                  16#A# => (24, 06, '\'), -- 10
                  16#B# => (25, 07, '\'), -- 11
                  16#C# => (26, 08, '|'), -- 12
                  16#D# => (26, 09, '|'), -- 13
                  16#E# => (26, 10, '|'), -- 14
                  16#F# => (25, 11, '*') --  Marker 15
                 ),  -- Sweep_00_14
               Sweep_15_29 =>
                 (
                  16#0# => (26, 11, '|'), -- 15
                  16#1# => (26, 12, '|'), -- 16
                  16#2# => (26, 13, '|'), -- 17
                  16#3# => (26, 14, '|'), -- 18
                  16#4# => (25, 15, '/'), -- 19
                  16#5# => (24, 16, '/'), -- 20
                  16#6# => (23, 17, '/'), -- 21
                  16#7# => (22, 18, '/'), -- 22
                  16#8# => (21, 19, '/'), -- 23
                  16#9# => (20, 20, '/'), -- 24
                  16#A# => (19, 21, '/'), -- 25
                  16#B# => (17, 22, '-'), -- 26
                  16#C# => (16, 22, '-'), -- 27
                  16#D# => (15, 22, '-'), -- 28
                  16#E# => (14, 22, '-'), -- 29
                  16#F# => (13, 21, '*') --  Marker 30
                 ),  -- Sweep_15_29
               Sweep_30_44 =>
                 (
                  16#0# => (13, 22, '-'), -- 30
                  16#1# => (12, 22, '-'), -- 31
                  16#2# => (11, 22, '-'), -- 32
                  16#3# => (10, 22, '-'), -- 33
                  16#4# => (09, 22, '-'), -- 34
                  16#5# => (07, 21, '\'), -- 35
                  16#6# => (06, 20, '\'), -- 36
                  16#7# => (05, 19, '\'), -- 37
                  16#8# => (04, 18, '\'), -- 38
                  16#9# => (03, 17, '\'), -- 39
                  16#A# => (02, 16, '\'), -- 40
                  16#B# => (01, 15, '\'), -- 41
                  16#C# => (00, 14, '|'), -- 42
                  16#D# => (00, 13, '|'), -- 43
                  16#E# => (00, 12, '|'), -- 44
                  16#F# => (01, 11, '*') --  Marker 45
                 ),  -- Sweep_30_44
               Sweep_45_59 =>
                 (
                  16#0# => (00, 11, '|'), -- 45
                  16#1# => (00, 10, '|'), -- 46
                  16#2# => (00, 09, '|'), -- 47
                  16#3# => (00, 08, '|'), -- 48
                  16#4# => (01, 07, '/'), -- 49
                  16#5# => (02, 06, '/'), -- 50
                  16#6# => (03, 05, '/'), -- 51
                  16#7# => (04, 04, '/'), -- 52
                  16#8# => (05, 03, '/'), -- 53
                  16#9# => (06, 02, '/'), -- 54
                  16#A# => (07, 01, '/'), -- 55
                  16#B# => (09, 00, '-'), -- 56
                  16#C# => (10, 00, '-'), -- 57
                  16#D# => (11, 00, '-'), -- 58
                  16#E# => (12, 00, '-'), -- 59
                  16#F# => (13, 01, '*') --  Marker 00
                 ),  -- Sweep_45_59
               Seconds_Drv =>
                 (
                  16#0# => (19, 06, '-'), -- Tens  a
                  16#1# => (20, 07, '|'), -- Tens  b
                  16#2# => (20, 09, '|'), -- Tens  c
                  16#3# => (19, 10, '-'), -- Tens  d
                  16#4# => (18, 09, '|'), -- Tens  e
                  16#5# => (18, 07, '|'), -- Tens  f
                  16#6# => (19, 08, '-'), -- Tens  g
                  16#7# => (20, 10, '.'), -- Tens  DP
                  16#8# => (23, 06, '-'), -- Units a
                  16#9# => (24, 07, '|'), -- Units b
                  16#A# => (24, 09, '|'), -- Units c
                  16#B# => (23, 10, '-'), -- Units d
                  16#C# => (22, 09, '|'), -- Units e
                  16#D# => (22, 07, '|'), -- Units f
                  16#E# => (23, 08, '-'), -- Units g
                  16#F# => (24, 10, ' ') --  Units DP
                 ),  -- Seconds_Drv
               Minutes_Drv =>
                 (
                  16#0# => (11, 06, '-'), -- Tens  a
                  16#1# => (12, 07, '|'), -- Tens  b
                  16#2# => (12, 09, '|'), -- Tens  c
                  16#3# => (11, 10, '-'), -- Tens  d
                  16#4# => (10, 09, '|'), -- Tens  e
                  16#5# => (10, 07, '|'), -- Tens  f
                  16#6# => (11, 08, '-'), -- Tens  g
                  16#7# => (12, 10, '.'), -- Tens  DP
                  16#8# => (15, 06, '-'), -- Units a
                  16#9# => (16, 07, '|'), -- Units b
                  16#A# => (16, 09, '|'), -- Units c
                  16#B# => (15, 10, '-'), -- Units d
                  16#C# => (14, 09, '|'), -- Units e
                  16#D# => (14, 07, '|'), -- Units f
                  16#E# => (15, 08, '-'), -- Units g
                  16#F# => (16, 10, '.') --  Units DP
                 ),  -- Minutes_Drv
               Hours_Drv =>
                 (
                  16#0# => (03, 06, '-'), -- Tens  a
                  16#1# => (04, 07, '|'), -- Tens  b
                  16#2# => (04, 09, '|'), -- Tens  c
                  16#3# => (03, 10, '-'), -- Tens  d
                  16#4# => (02, 09, '|'), -- Tens  e
                  16#5# => (02, 07, '|'), -- Tens  f
                  16#6# => (03, 08, '-'), -- Tens  g
                  16#7# => (04, 10, '.'), -- Tens  DP
                  16#8# => (07, 06, '-'), -- Units a
                  16#9# => (08, 07, '|'), -- Units b
                  16#A# => (08, 09, '|'), -- Units c
                  16#B# => (07, 10, '-'), -- Units dStatus.AL_Test_Value
                  16#C# => (06, 09, '|'), -- Units e
                  16#D# => (06, 07, '|'), -- Units f
                  16#E# => (07, 08, '-'), -- Units g
                  16#F# => (08, 10, '.') --  Units DP
                 ),  -- Hours_Drv
               Years_Drv =>
                 (
                  16#0# => (23, 11, '-'), -- Units a
                  16#1# => (24, 12, '|'), -- Units b
                  16#2# => (24, 14, '|'), -- Units c
                  16#3# => (23, 15, '-'), -- Units d
                  16#4# => (22, 14, '|'), -- Units e
                  16#5# => (22, 12, '|'), -- Units f
                  16#6# => (23, 13, '-'), -- Units g
                  16#7# => (24, 15, '.'), -- Units DP
                  16#8# => (19, 11, '-'), -- Tens  a
                  16#9# => (20, 12, '|'), -- Tens  b
                  16#A# => (20, 14, '|'), -- Tens  c
                  16#B# => (19, 15, '-'), -- Tens  d
                  16#C# => (18, 14, '|'), -- Tens  e
                  16#D# => (18, 12, '|'), -- Tens  f
                  16#E# => (19, 13, '-'), -- Tens  g
                  16#F# => (20, 15, '.') --  Tens  DP
                 ),  -- Years_Drv
               Months_Drv =>
                 (
                  16#0# => (15, 11, '-'), -- Units a
                  16#1# => (16, 12, '|'), -- Units b
                  16#2# => (16, 14, '|'), -- Units c
                  16#3# => (15, 15, '-'), -- Units d
                  16#4# => (14, 14, '|'), -- Units e
                  16#5# => (14, 12, '|'), -- Units f
                  16#6# => (15, 13, '-'), -- Units g
                  16#7# => (16, 15, '.'), -- Units DP
                  16#8# => (11, 11, '-'), -- Tens  a
                  16#9# => (12, 12, '|'), -- Tens  b
                  16#A# => (12, 14, '|'), -- Tens  c
                  16#B# => (11, 15, '-'), -- Tens  d
                  16#C# => (10, 14, '|'), -- Tens  e
                  16#D# => (10, 12, '|'), -- Tens  f
                  16#E# => (11, 13, '-'), -- Tens  gStatus.AL_Test_Value
                  16#F# => (12, 15, '.') --  Tens  DP
                 ),  -- Months_Drv
               Days_Drv =>
                 (
                  16#0# => (07, 11, '-'), -- Units a
                  16#1# => (08, 12, '|'), -- Units b
                  16#2# => (08, 14, '|'), -- Units c
                  16#3# => (07, 15, '-'), -- Units d
                  16#4# => (06, 14, '|'), -- Units e
                  16#5# => (06, 12, '|'), -- Units f
                  16#6# => (07, 13, '-'), -- Units g
                  16#7# => (08, 15, '.'), -- Units DP
                  16#8# => (03, 11, '-'), -- Tens  a
                  16#9# => (04, 12, '|'), -- Tens  b
                  16#A# => (04, 14, '|'), -- Tens  c
                  16#B# => (03, 15, '-'), -- Tens  d
                  16#C# => (02, 14, '|'), -- Tens  e
                  16#D# => (02, 12, '|'), -- Tens  f
                  16#E# => (03, 13, '-'), -- Tens  g
                  16#F# => (04, 15, '.') --  Tens  DP
                 )  -- Days_Drv
              ); -- LED_DataStatus.AL_Test_Value

         begin -- Animate_Clock
            for D in LED_Drivers loop
               for C in LED_Channels loop
                  if Full_Update or else
                    (Status.LED_Array (D, C) xor
                         Previous_Status.LED_Array (D, C)) then
                     Goto_XY (LED_Data (D, C).X + Origin_X,
                              LED_Data (D, C).Y + Origin_Y);
                     if Status.LED_Array (D, C) then
                        Put (LED_Data (D, C).Active);
                     else
                        Put (Passive);
                     end if; -- Status.LED_Array (D, C)
                  end if; --  Full_Update or else ...
               end loop; -- C in LED_Channels
            end loop; -- D in LED_Drivers
         end Animate_Clock;

         Origin_X : constant X_Pos := 27;
         -- Starting position of clock animation
         Clock_Version_Y : constant Y_Pos := Y_Pos'First;
         Time_Y : constant Y_Pos := Clock_Version_Y + 4;
         Chime_Toggle_Y : constant Y_Pos := Time_Y + 1;
         Chime_Enabled_Y : constant Y_Pos := Chime_Toggle_Y + 1;
         Chime_Volume_Y : constant Y_Pos := Chime_Enabled_Y + 1;
         Commands_Y : constant Y_Pos := Chime_Volume_Y + 2;
         Prompt_Y : constant Y_Pos := Commands_Y + 9;
         Diagnostic_Y : constant Y_Pos := Prompt_Y + 2;
         Full_Update : Boolean;

      begin -- Receive_Response
         Boolean_IO.Default_Setting := Lower_Case;
         Full_Update := Status.Request /= Previous_Status.Request or
           (Status.Diagnostic_Toggle xor Previous_Status.Diagnostic_Toggle);
         if Full_Update then
            Clear_Screen;
         end if; -- Full_Update
         Set_Cursor (False); -- hide cursor
         if Full_Update then
            Goto_XY (X_Pos'First, Clock_Version_Y);
            Put ("Clock Version: " & Status.Clock_Version);
            Goto_XY (X_Pos'First, Clock_Version_Y + 1);
            Put ("UI Interface:  " & Status.User_Interface_Version);
            Goto_XY (X_Pos'First, Clock_Version_Y + 2);
            Put ("UI Build:      20220609");
         end if; -- Full_Update
         Goto_XY (X_Pos'First, Time_Y);
         Put (Image (Status.Current_Time, True,
              UTC_Time_Offset (Status.Current_Time)));
         if Full_Update or else
           Status.Chime_Toggle /= Previous_Status.Chime_Toggle then
            Goto_XY (X_Pos'First, Chime_Toggle_Y);
            Put ("User Chiming Control: ");
            Boolean_IO.Put (Status.Chime_Toggle, 5);
         end if; -- Full_Update or else ...
         if Full_Update or else
           Status.Chime_Enabled /= Previous_Status.Chime_Enabled then
            Goto_XY (X_Pos'First, Chime_Enabled_Y);
            Put ("Chiming Enabled:      ");
            Boolean_IO.Put (Status.Chime_Enabled, 5);
         end if; -- Full_Update or else ...
         if Full_Update or else
           Status.Chime_Volume /= Previous_Status.Chime_Volume then
            Goto_XY (X_Pos'First, Chime_Volume_Y);
            Put ("Chiming Volume:      " & Status.Chime_Volume'Img & '%');
         end if; -- Full_Update or else ...
         if Full_Update then
            Goto_XY (X_Pos'First, Commands_Y);
            Put ("Command Options");
            Goto_XY (X_Pos'First, Commands_Y + 1);
            Put ("Chime toggle");
            Goto_XY (X_Pos'First, Commands_Y + 2);
            Put ("+ volume up");
            Goto_XY (X_Pos'First, Commands_Y + 3);
            Put ("- volume down");
            Goto_XY (X_Pos'First, Commands_Y + 4);
            Put ("Test volume");
            Goto_XY (X_Pos'First, Commands_Y + 5);
            Put ("Step sweep mode");
            Goto_XY (X_Pos'First, Commands_Y + 6);
            Put ("Diagnostic toggle");
            Goto_XY (X_Pos'First, Commands_Y + 7);
            Put ("Exit user interface");
         end if; -- Full_Update
         Animate_Clock (Origin_X, Y_Pos'First, Status, Previous_Status,
                        Full_Update);
         if Status.Diagnostic_Toggle then
            Goto_XY (X_Pos'First, Diagnostic_Y);
            Put ("Diagnostic Information");
            if Full_Update or else
              Status.Ambient_Light /= Previous_Status.Ambient_Light then
               Goto_XY (X_Pos'First, Diagnostic_Y + 1);
               Put ("Measured Ambient Light:");
               Greyscale_IO.Put (Status.Ambient_Light, 7);
            end if; -- Full_Update or else ...
            if Full_Update or else
              Status.AL_Test_Value /= Previous_Status.AL_Test_Value then
               Goto_XY (X_Pos'First, Diagnostic_Y + 2);
               Put ("Ambient Light Test Value:");
               Greyscale_IO.Put (Status.AL_Test_Value, 5);
            end if; -- Full_Update or else ...
            if Full_Update or else
              Status.Current_Item /= Previous_Status.Current_Item then
               Goto_XY (X_Pos'First, Diagnostic_Y + 3);
               Put (Status.Current_Item);
            end if; -- Full_Update or else ...
         end if; -- Status.Diagnostic_Toggle
         Goto_XY (X_Pos'First, Prompt_Y);
         Put ("Command>");
         case Status.Request is
            when Volume_Up | Volume_Down =>
               Process_Requests.Quick_Response;
            when Exit_User_Interface =>
               Clear_Screen;
               Goto_XY (X_Pos'First, Y_Pos'First);
               Put_Line ("Exiting User Interface");
               delay 3.0;
               Run_Screen_Update := False;
            when others  =>
               null;
         end case;
         Set_Cursor (True); -- Display cursor
      end Receive_Response;

      Status : Status_Records;
      RX_Buffer : Response_Buffers;
      for RX_Buffer'Address use Status'Address;
      pragma Import (Ada, RX_Buffer);
      Last : Stream_Element_Offset;
      Previous_Status : Status_Records;

   begin -- Update_Screen
      Create_Socket (RX_Socket, Family_Inet, Socket_Datagram); -- UDP socket
      Bind_Socket (RX_Socket, Client_Address);
      Previous_Status.Request := Exit_User_Interface;
      Previous_Status.Current_Time := Clock - 10.0;
      -- Ensures full initialisation of Previous_Status with little to no
      -- possibilit of matching current values.
      Clear_Screen;
      Process_Requests.RX_Ready;
      while Run_Screen_Update loop
         Receive_Socket (RX_Socket, RX_Buffer, Last);
         if Status.User_Interface_Version = Interface_Version then
            Receive_Response (Status, Previous_Status);
            Previous_Status := Status;
         else
            Put ("Server version: " & Status.User_Interface_Version &
                   " does not match client version " & Interface_Version);
            Process_Requests.Version_Mismatch;
            Run_Screen_Update := False;
            Bleep;
            delay 10.0;
         end if; -- Status.User_Interface_Version = Interface_Version
      end loop;
      Close_Socket (RX_Socket);
      Clear_Screen;
      Goto_XY (X_Pos'First,Y_Pos'First);
      accept Finished;
   end Update_Screen;

end User_Interface_Client;
