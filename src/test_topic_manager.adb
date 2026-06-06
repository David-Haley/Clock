--  This program tests Topic_Manage and indirectly the Topic_Editor.
--  It requires a configuration file with fields Test_String, Test_Number,
--  Test_U16, Test_U32 and Test_Boolean defined. The test program assumes that
--  the the Broker, User and Password can be used for both publication and
--  subscription. The configuration file needs to define the Item_Ids listed
--  Display below.

--  Author    : David Haley
--  Created   : 08/05/2026
--  Last Edit : 05/06/2026

--  20260605: Comprehensive testing including fornatting of Unsigned_16,
--  Unsigned_32 and Boolean items. Effectively a full rewrite!
--  20260603: Merged Topic Manager.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Interfaces; use Interfaces;
with MQTT_Client; use MQTT_Client;
with Topic_Manager; use Topic_Manager;

procedure Test_Topic_Manager is

   Topic : constant Topics := To_Unbounded_String ("Test");

   U16P : constant Unsigned_16 := 12345;
   U16N : constant Unsigned_16 := not U16P + 1;
   U32P : constant Unsigned_32 := 123456;
   U32N : constant Unsigned_32 := not U32P + 1;

   Message_A : constant MQTT_Data :=
      "{" &
         """Test_String"" : ""QWERTY""," &
         """Test_Number"" : 123456 ," &
         """Test_U16"" : " & U16P'Img & " , " &
         """Test_U32"" : " & U32P'Img & " , " &
         """Test_Boolean"" : true ," &
         """Time_Sent"" : """;

   Message_B : constant MQTT_Data :=
      "{" &
         """Test_String"" : ""ASDFGH""," &
         """Test_Number"" : -12345," &
         """Test_U16"" : " & U16N'Img & " , " &
         """Test_U32"" : " & U32N'Img & " , " &
         """Test_Boolean"" : false ," &
         """Time_Sent"" : """;

   Message_End : constant MQTT_Data := """}";

   procedure Commands is

   begin -- Commands
      Put_Line ("Available commands:");
      Put_Line ("A : Publish message A.");
      Put_Line ("B : Publish message B.");
      Put_Line ("D : Display received message.");
      Put_Line ("Q : Quit");
      Put_Line ("? : List available commands.");
      New_Line;
   end Commands;

   procedure Publish (Tx_Hadle : in MQTT_Handle;
                      Message_Start, Message_End : in MQTT_Data) is

      Message : constant MQTT_Data := Message_Start &
        Local_Image (Clock) & Message_End;

   begin -- Publish
      Put_Line ("Publishing:");
      Send (Tx_Hadle, Message);
      Put_Line ("Sent : " & Message);
      New_Line;
   end Publish;

   procedure Display is

      --  The following items must be defined in the configuration file:

      Publish_Time : constant MQTT_Item_Ids := "Publish_Time";
      String_Test : constant MQTT_Item_Ids := "String_Test";
      Nunber_Test : constant MQTT_Item_Ids := "Number_Test";
      U16_0 : constant MQTT_Item_Ids := "U16_0";
      U16_1 : constant MQTT_Item_Ids := "U16_1";
      U16_2 : constant MQTT_Item_Ids := "U16_2";
      U16_3 : constant MQTT_Item_Ids := "U16_3";
      U32_0 : constant MQTT_Item_Ids := "U32_0";
      U32_1 : constant MQTT_Item_Ids := "U32_1";
      U32_2 : constant MQTT_Item_Ids := "U32_2";
      U32_3 : constant MQTT_Item_Ids := "U32_3";
      Boolean_Test : constant MQTT_Item_Ids := "Boolean_Test";

   begin -- Display
      Put_Line ("Display formatted data");
      Put_Line (Publish_Time & ": """ & Get_For_Display (Publish_Time) & """");
      Put_Line (String_Test & ": """ & Get_For_Display (String_Test) & """");
      Put_Line (Nunber_Test & ": """ & Get_For_Display (Nunber_Test) & """");
      Put_Line (U16_0 & ": """ & Get_For_Display (U16_0) & """");
      Put_Line (U16_1 & ": """ & Get_For_Display (U16_1) & """");
      Put_Line (U16_2 & ": """ & Get_For_Display (U16_2) & """");
      Put_Line (U16_3 & ": """ & Get_For_Display (U16_3) & """");
      Put_Line (U32_0 & ": """ & Get_For_Display (U32_0) & """");
      Put_Line (U32_1 & ": """ & Get_For_Display (U32_1) & """");
      Put_Line (U32_2 & ": """ & Get_For_Display (U32_2) & """");
      Put_Line (U32_3 & ": """ & Get_For_Display (U32_3) & """");
      Put_Line (Boolean_Test & ": """ & Get_For_Display (Boolean_Test) & """");
      New_Line;
   end Display;

   Tx_Handle :MQTT_Handle;
   Command : Character := '?';
   
begin -- Test_Topic_Manager
   Put_Line ("Test_Topic_Manager version 20260605");
   Read_Topics;
   Connect_Tx (To_String (Get_Broker (Topic)),
               To_String (Get_User (Topic)),
               To_String (Get_Password (Topic)),
               To_String (Topic),
               Tx_Handle);
      while not Is_Connected_Tx (Tx_Handle) loop
         Put_Line ("Connecting to " & Topic);
         delay 0.1;
      end loop; -- not Is_Connected_Tx (Tx_Handle)
      Put_Line ("Connected to " & Topic);
   while Command /= 'q' and Command /= 'Q' loop
      case Command is
      when 'a' | 'A' =>
         Publish (Tx_Handle, Message_A, Message_End);
      when 'b' | 'B' =>
         Publish (Tx_Handle, Message_B, Message_End);
      when 'd' | 'D' =>
         Display;
      when '?' =>
         Commands;
      when others =>
         Put_Line ("Invalid command");
      end case; -- Command
      Put ("Command [A | B | D | Q | ?]: ");
      Get_Immediate (Command);
      New_Line;
   end loop; -- Command /= 'q' and Command /= 'Q'
   Disconnect (Tx_Handle);
end  Test_Topic_Manager;