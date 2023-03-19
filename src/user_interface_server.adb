-- This package provides server component for a locally distributed IOT_Clock
-- user interface.

-- Author    : David Haley
-- Created   : 24/07/2019
-- Last Edit : 22/09/2022

-- 20220922 : Termination mechanism changed to use abort.
-- 20220920 : User initiated shutdown moved to main loop.
-- 20220609 : User interface shutdown removed now by SIGTERM or Ctrl_C.
-- 20220127 : Server unconditionally replies in the event of a version mismatch
-- so that the version is reported to the client.
-- 20220126 : Pass through of Diagnostic_Toggle added.
-- 20220125 : Provision for reporting to the user interface and local storage to
-- improve efficiency of handling multiple cliants.
-- 2022020 : Volume_Test added;
-- 20220119 : Get_Chime_Toggle added. Decoupling user interface from main loop
-- timing;
-- 20220116 : Implementation of chiming control via user interface.

with Ada.Streams; use Ada.Streams;
with Interfaces; use Interfaces;
with GNAT.Sockets; use GNAT.Sockets;
with Clock_Driver; use Clock_Driver;
with Shared_User_Interface; use Shared_User_Interface;
with Secondary_Display; use Secondary_Display;
with Chime; use Chime;

package body User_Interface_Server is

   use Clock_LEDs;

   protected UI_Data is

      function Get_Chime_Toggle return Boolean;
      -- Returns true if chiming is turned on in UI, defaults to true on
      -- startup.

      procedure Report_Chiming (Chiming_Enabled : in Boolean;
                                Chime_Volume : in Chime_Volumes);
      -- Provides for reporting of current chime state to user interface.

      procedure Toggle_Chime;
      -- Inverts the current state of Chime_Toggle.

      procedure Report_Time (Current_Time : in Time);
      -- Provides for reporting of time to the User Interface.

      procedure Report_Current_Item (Current_Item : UI_Strings);
      -- Provides for reporting of the item being displayed on the secondary
      -- display.

      procedure Report_Ambient_Light (Ambient_Light,
                                      AL_Test_Value : in Greyscales);
      -- Provides for reporting of current ambient light and the current
      -- comparison value to user interface.

      procedure Report_LED (Driver : in LED_Drivers;
                            Channel : in LED_Channels;
                            Greyscale : in Greyscales);
      -- Provides for reporting of LED state  to the User Interface.

      procedure Report_Clock_Version (Clock_Version : Version_String);
      -- Used internally to initialise the locally stored Clock_Version

      function Get_Clock_Status return Status_Records;
      -- returns the copy of the current clock state stored in UI_Data.

   private

      Clock_Status : Status_Records;

   end UI_Data;

   task UI_Server is
   end UI_Server;

   function Get_Chime_Toggle return Boolean is (UI_Data.Get_Chime_Toggle);

      -- Returns true if chiming is turned on in UI, defaults to true on
      --startup.

   procedure Report_Chiming (Chiming_Enabled : in Boolean;
                             Chime_Volume : in Chime_Volumes) is

      -- Provides for reporting of current chime state to user interface.

   begin -- Report_Chiming
      UI_Data.Report_Chiming (Chiming_Enabled,Chime_Volume);
   end Report_Chiming;


    procedure Report_Time (Current_Time : in Time) is
      -- Provides for reporting of time to the User Interface.

   begin -- Primary_Time
      UI_Data.Report_Time (Current_Time);
   end;

   procedure Report_Current_Item (Current_Item : UI_Strings) is

      -- Provides for reporting of the item being displayed on the secondary
      -- display.

   begin -- Report_Current_Item
      UI_Data.Report_Current_Item (Current_Item);
   end Report_Current_Item;

   procedure Report_Ambient_Light (Ambient_Light,
                                   AL_Test_Value : in Greyscales) is

      -- Provides for reporting of current ambient light and the current
      -- comparison value to user interface.

   begin -- Report_Ambient_Light
      UI_Data.Report_Ambient_Light (Ambient_Light, AL_Test_Value);
   end Report_Ambient_Light;

   procedure Report_LED (Driver : in LED_Drivers;
                         Channel : in LED_Channels;
                         Greyscale : in Greyscales) is

      -- Provides for reporting of LED state to the User Interface.

   begin -- Report_LED
      UI_Data.Report_LED (Driver, Channel, Greyscale);
   end Report_LED;

   procedure Stop_UI_Server is

   -- Terminate user interface server

   begin -- Stop_UI_Server
      abort UI_Server;
   end Stop_UI_Server; 

   task body UI_Server is

      RX_Socket, TX_Socket : Socket_Type;
      Server_Address : Sock_Addr_Type := (Family => Family_Inet,
                                          Addr => Any_Inet_Addr,
                                          Port => Request_Port);
      Client_Address : Sock_Addr_Type;
      Request_Record : Request_Records;
      RX_Buffer : Request_Buffers;
      for RX_Buffer'Address use Request_Record'Address;
      pragma Import (Ada, RX_Buffer);
      Clock_Status : Status_Records;
      Status_TX_Buffer : Response_Buffers;
      for Status_TX_Buffer'Address use Clock_Status'Address;
      pragma Import (Ada, Status_TX_Buffer);
      Last : Stream_Element_Offset;

   begin -- UI_Server
      UI_Data.Report_Clock_Version (Clock_Version);
      Create_Socket (RX_Socket, Family_Inet, Socket_Datagram);
      Bind_Socket (RX_Socket, Server_Address);
      Create_Socket (TX_Socket, Family_Inet, Socket_Datagram);
      loop -- Server
         Receive_Socket (RX_Socket, RX_Buffer, Last, Client_Address);
         -- Returns after after Receive_Timeout or when packet received.
         if Request_Record.User_Interface_Version = Interface_Version then
            -- only action commands with matching interface version
            Client_Address.Port := Response_Port;
            case Request_Record.Request is
            when Toggle_Chime =>
               UI_Data.Toggle_Chime;
            when Volume_Up =>
               Raise_Volume;
            when Volume_Down =>
               Lower_Volume;
            when Volume_Test =>
               Test_Volume;
            when others =>
               null;
            end case; -- Request_Record.Request
         end if; -- Request_Record.User_Interface_Version = Interface_Version
         -- Unconditionally reply so that any version mismatch is reported to
         -- client.
         Clock_Status := UI_Data.Get_Clock_Status;
         Clock_Status.Request := Request_Record.Request;
         Clock_Status.Diagnostic_Toggle := Request_Record.Diagnostic_Toggle;
         Send_Socket (TX_Socket, Status_TX_Buffer, Last, Client_Address);
      end loop; -- Server 
   end UI_Server;

   protected body UI_Data is

      function Get_Chime_Toggle return Boolean is
        (UI_Data.Clock_Status.Chime_Toggle);

         -- Returns true if chiming is turned on in UI, defaults to true on
         --startup.

      procedure Report_Chiming (Chiming_Enabled : in Boolean;
                                Chime_Volume : in Chime_Volumes) is

         -- Provides for reporting of current chime state to user interface.

      begin -- Report_Chiming
         UI_Data.Clock_Status.Chime_Enabled := Chiming_Enabled;
         UI_Data.Clock_Status.Chime_Volume := Chime_Volume;
      end Report_Chiming;

      procedure Toggle_Chime is

         -- Inverts the current state of Chime_Toggle.

      begin -- Toggle_Chime
         Clock_Status.Chime_Toggle := not Clock_Status.Chime_Toggle;
      end Toggle_Chime;

      procedure Report_Time (Current_Time : in Time) is

         -- Provides for reporting of time to the User Interface.

      begin -- Report_Time
         Clock_Status.Current_Time := Current_Time;
      end Report_Time;

      procedure Report_Current_Item (Current_Item : UI_Strings) is

         -- Provides for reporting of the item being displayed on the secondary
         -- display.

      begin -- Report_Current_Item
         UI_Data.Clock_Status.Current_Item := Current_Item;
      end Report_Current_Item;

      procedure Report_Ambient_Light (Ambient_Light,
                                      AL_Test_Value : in Greyscales) is

         -- Provides for reporting of current ambient light and the current
         -- comparison value to user interface.

      begin --
         UI_Data.Clock_Status.Ambient_Light := Ambient_Light;
         UI_Data.Clock_Status.AL_Test_Value := AL_Test_Value;
      end;

      procedure Report_LED (Driver : in LED_Drivers;
                            Channel : in LED_Channels;
                            Greyscale : in Greyscales) is

         -- Provides for reporting of LED state to the User Interface.

      begin -- Report_LED
         UI_Data.Clock_Status.LED_Array (Driver, Channel) :=
           Greyscale > Greyscales'First;
      end Report_LED;

      procedure Report_Clock_Version (Clock_Version : Version_String) is

         -- Used internally to initialise the locally stored Clock_Version

      begin -- Report_Clock_Version
         UI_Data.Clock_Status.Clock_Version := Clock_Version;
      end Report_Clock_Version;

      function Get_Clock_Status return Status_Records is (UI_Data.Clock_Status);

         -- returns the copy of the current clock state stored in UI_Data.

   end UI_Data;

end User_Interface_Server;
