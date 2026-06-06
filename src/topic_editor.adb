--  This program is an interactive editor for IOT_Clock topic files.

--  Author    : David Haley
--  Created   : 08/05/2026
--  Last Edit : 02/06/2026

--  20260602: Merged with subscription editor

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Topic_Manager; use Topic_Manager;

procedure Topic_Editor is

   Zero_Length, UI_Error : exception;

   procedure Subscription_Editor is

      procedure Get_Susbcription (Topic : out Topics;
                                  Broker : out Brokers;
                                  User : out Users;
                                  Password : out Passwords) is

      begin -- Get_Susbcription
         Put ("Topic: ");
         Get_Line (Topic);
         Put ("Broker: ");
         Get_Line (Broker);
         Put ("User: ");
         Get_Line (User);
         Put ("Password: ");
         Get_Line (Password);
      end Get_Susbcription;

      procedure List_Commands is

      begin -- List_Commands
         Put_Line ("Available commands are listed as follows:");
         Put_Line ("A : Add a new topic.");
         Put_Line ("E: Exit subscription editor");
         Put_Line ("L : List stored topic information.");
         Put_Line ("M : Modify the information stored for an existing topic.");
         Put_Line ("P : Check password, compares stored password with user " &
                  "entry.");
         Put_Line ("X : Delete an existing topic.");
         Put_Line ("? : List commands.");
      end List_Commands;
   
      Command : Character := '?';
      Broker : Brokers;
      User : Users;
      Password : Passwords;
      Topic : Topics;
   
   begin -- Subscription_Editor
      while Command /= 'e' and Command /= 'E' loop
         case Command is
            when 'a' | 'A' =>
               Put_Line ("Add a new topic.");
               Get_Susbcription (Topic, Broker, User, Password);
               if Topic_Exists (Topic) then
                  Put_Line ("Topic already exists, use Modify to change.");
               elsif Length (Topic) > 0 and Length (Broker) > 0 and
               Length (User) > 0
               then
                  Create_Subscription (Topic, Broker, User, Password);
               else
                  Put_Line ("Zero length strings not permitted, nothing changed.");
               end if; -- Topic_Exists (Topic)
            when 'l' | 'L' =>
               Put_Subscriptions;
            when 'm' | 'M' =>
               Put_Line ("Modify the information stored for a topic.");
               Put_Line ("Enter for no change or new value and enter to change.");
               Get_Susbcription (Topic, Broker, User, Password);
               if Topic_Exists (Topic) then
                  if Length (Broker) = 0 and Length (User) = 0 and
                  Length (Password) = 0
                  then
                     Put_Line ("No values entered, nothing changed");
                  else
                     if Length (Password) = 0 then
                        Password := Get_Password (Topic);
                     end if; -- Length (Password) = 0
                     Modify_Subscription (Topic, Broker, User, Password);
                  end if; -- Length (Broker) = 0 and Length (User) = 0 and ...
               else
                  Put_Line ("Topic not found, nothing changed.");
               end if; -- Topic_Exists (Topic)
            when 'p' | 'P' =>
               Put_Line ("Check Password");
               Put ("Topic: ");
               Get_Line (Topic);
               Put ("Password: ");
               Get_Line (Password);
               if Topic_Exists (Topic) then
                  if Get_Password (Topic) = Password then
                     Put_Line ("Passwords match");
                  else
                     Put_Line ("Passwords do not match");
                  end if; -- Get_Password (Topic) = Password
               else
                  Put_Line ("Topic not found, password connot be checked.");
               end if; -- Topic_Exists (Topic)
            when 'x' | 'X' =>
               Put_Line ("Delete Topic, enter topic to be deleted.");
               Put ("Topic: ");
               Get_Line (Topic);
               if Topic_Exists (Topic) then
                  Delete_Subscription (Topic);
               else
                  Put_Line ("Topic not found, nothing changed.");
               end if; -- Topic_Exists (Topic)
            when '?' =>
               List_Commands;
            when others =>
               Put_Line ("Invalid command");
         end case; -- Command
         Put ("Command [A | E | L | M | P | X | ?]: ");
         Get_Immediate (Command);
         New_Line;
      end loop; -- Command /= 'q' and Command /= 'Q'
   end Subscription_Editor;

   procedure Item_Editor is

      procedure Create_Item (Item_Id : MQTT_Item_Ids) is

         procedure Topic_and_Field (Topic : out Topics;
                                    Field : out Fields) is

         begin -- Topic_and_Field
            Put ("Topic: ");
            Get_Line (Topic);
            Put ("Field: ");
            Get_Line (Field);
            if Length (Topic) = 0 or Length (Field) = 0 then
               raise Zero_Length;
            end if; -- Length (Topic) = 0 or Length (Field) = 0
            if not Topic_Exists (Topic) then
               raise UI_Error with "Topic " & To_String (Topic) &
               " does not exist";
            end if; -- not Topic_Exists (Topic)
         end Topic_and_Field;

         procedure Unsigned_Formatting (Scaling_Factor : out Scaling_Factors;
                                        Decimal_Place : out Decimal_Places;
                                        Is_Signed : out Boolean) is

            User_Input : Unbounded_String;

         begin -- Unsigned_Formatting
            Put ("Scaling Factor [" & 
               Scaling_Factors'Image (Scaling_Factors'First) & " .." &
               Scaling_Factors'Image (Scaling_Factors'Last) & " ] : ");
            User_Input := Get_Line;
            begin -- Scaling factor exception block
               Scaling_Factor := Scaling_Factors'Value (To_String (User_Input));
            exception
               when others =>
                  raise UI_Error with "Bad input for Scaling_Factor";
            end; -- Scaling factor exception block
            Put ("Decimal Places [" & 
               Decimal_Places'Image (Decimal_Places'First) & " .." &
               Decimal_Places'Image (Decimal_Places'Last) & " ] : ");
            User_Input := Get_Line;
            begin -- Decimal place exception block
               Decimal_Place := Decimal_Places'Value (To_String (User_Input));
            exception
               when others =>
                  raise UI_Error with "Bad input for Decomal_Place";
            end; -- Decimal place exception block
            Put ("Is Signed [ true | false ] : ");
            User_Input := Get_Line;
            begin -- Is signed exception block
               Is_Signed := Boolean'Value (To_String (User_Input));
            exception
               when others =>
                  raise UI_Error with "Bad input for Is_Signed";
            end; -- Is signed exception block
         end Unsigned_Formatting;

         procedure True_and_False (True_Text : out Unbounded_String;
                                   False_Text : out Unbounded_String) is

         begin -- True_and_False
            Put ("True Text [ Enter = ""true"" | text & Enter ] : ");
            True_Text := Get_Line;
            Put ("False Text [ Enter = ""false"" | text & Enter ] : ");
            False_Text := Get_Line;
            if Length (True_Text) = 0 then
               True_Text := To_Unbounded_String ("true");
            end if; -- Length (True_Text) = 0
            if Length (False_Text) = 0 then
               False_Text := To_Unbounded_String ("false");
            end if; -- Length (False_Text) = 0
         end True_and_False;

         Command : Character;
         Topic : Topics;
         Field : Fields;
         True_Text, False_Text : Unbounded_String;
         Scaling_Factor : Scaling_Factors;
         Decimal_Place : Decimal_Places;
         Is_Signed : Boolean;

      begin -- Create_Item
         Put_Line ("Item Menu");
         Put_Line ("B Boolean");
         Put_Line ("N : Number");
         Put_Line ("S : String");
         Put_Line ("1 : Unsigned 16 to be scaled");
         Put_Line ("3 : Unsigned 32 to be scaled");
         Put ("Command [B | N | S | 1 | 3]: ");
         Get_Immediate (Command);
         New_Line;
         case Command is
            when 's' | 'S' =>
               Topic_and_Field (Topic, Field);
               Create_String_Item (Item_Id, Topic,Field);
            when 'n' | 'N' =>
               Topic_and_Field (Topic, Field);
               Create_Number_Item (Item_Id, Topic, Field);
            when '1' =>
               Topic_and_Field (Topic, Field);
               Unsigned_Formatting (Scaling_Factor, Decimal_Place, Is_Signed);
               Create_Scaled_Number_Item (Item_Id, Topic, Field,
                                          MQTT_U16,
                                          Scaling_Factor,
                                          Decimal_Place,
                                          Is_Signed);
            when '3' =>
               Topic_and_Field (Topic, Field);
               Unsigned_Formatting (Scaling_Factor, Decimal_Place, Is_Signed);
               Create_Scaled_Number_Item (Item_Id, Topic, Field,
                                          MQTT_U32,
                                          Scaling_Factor,
                                          Decimal_Place,
                                          Is_Signed);
            when 'b' | 'B' =>
               Topic_and_Field (Topic, Field);
               True_and_False (True_Text, False_Text);
               Create_Boolean_Item (Item_Id, Topic, Field,
                                    True_Text,
                                    False_Text);
            when others =>
               Put_Line ("Invalid command for Item Id creation");
         end case;
      exception
         when Zero_Length =>
            Put_Line ("Zero length string entries are not permmited");
            Put_Line ("New item has not been created.");
         when E: UI_Error =>
            Put_Line (Exception_Message (E));
            Put_Line ("New item has not been created.");
      end Create_Item;

      function Get_Item_Id return Unbounded_String is

      begin -- Get_Item_Id
         Put ("Item Id: ");
         return Get_Line;
      end Get_Item_Id;

      procedure List_Commands is

      begin -- List_Commands
         Put_Line ("Available commands are as follows:");
         Put_Line ("A : Add a new Item Id.");
         Put_Line ("E : Exit item editor");
         Put_Line ("D : Display the stored configuration for one Item Id");
         Put_Line ("L : List all stored Item Ids.");
         Put_Line ("X : Delete an existing Item Id.");
         Put_Line ("? : List commands.");
      end List_Commands;

      Item_Id_U : Unbounded_String;
      Command : Character := '?';

   begin -- Item_Editor
      while Command /= 'e' and Command /= 'E' loop
         case Command is
            when 'a' | 'A' =>
               Put_Line ("Add a new Item Id.");
               Item_Id_U := Get_Item_Id;
               if Item_Id_Exists (To_String (Item_Id_U)) then
                  Put_Line ("Item Id already exists.");
               elsif Length (Item_Id_U) > 0 then
                  Create_Item (To_String (Item_Id_U));
               else
                  Put_Line ("Zero length Item Id is not permitted.");
               end if; -- Topic_Exists (To_String (Topic))
            when 'd' | 'D' =>
               Put_Line ("Display data for one Item Id");
               Item_Id_U := Get_Item_Id;
               if Item_Id_Exists (To_String (Item_Id_U)) then
                  Put_Item (To_String (Item_Id_U));
               else
                  Put_Line ("Item Id not found, nothing to display.");
               end if; -- Topic_Exists (To_String (Topic))
            when 'l' | 'L' =>
               Put_Line ("List all Item Id with topic and field");
               List_Items;
            when 'x' | 'X' =>
               Put_Line ("Delete Item Id");
               Item_Id_U := Get_Item_Id;
               if Item_Id_Exists (To_String (Item_Id_U)) then
                  Delete_Item (To_String (Item_Id_U));
               else
                  Put_Line ("Item Id not found, nothing changed.");
               end if; -- Topic_Exists (To_String (Topic))
            when '?' =>
               List_Commands;
            when others =>
               Put_Line ("Invalid command");
         end case; -- Command
         Put ("Command [A | D | E | L | X | ?]: ");
         Get_Immediate (Command);
         New_Line;
      end loop; -- Command /= 'q' and Command /= 'Q'
   end Item_Editor;
   
   Answer, Command : Character;
   
begin -- Topic_Editor
   Put_Line ("IOT_Clock Topic Editor version 2026602");
   if Topic_Manager.File_Exists then
      Read_Topics;
      Put_Line ("Topic file read");
      Command := '?';
   else
      Put ("Topic management file not found, create a new one [Y | N]: ");
      Get_Immediate (Answer);
      New_Line;
      if Answer = 'y' or Answer = 'Y' then
         Put_Line ("A new topic file will be created");
         Command := '?';
      else
         Command := 'Q';
      end if; -- Answer = 'y' or Answer = 'Y'
   end if; -- Topic_Manager.File_Exists
   while Command /= 'q' and Command /= 'Q' loop
      case Command is
         when 'i' | 'I' =>
            Put_Line ("Starting Item editor.");
            Item_Editor;
         when 's' | 'S' =>
            Write_Topics;
            Put_Line ("Topic has been file written");
         when 't' | 'T' =>
            Put_Line ("Starting topic editor.");
            Subscription_Editor;
         when '?' =>
            Put_Line ("Available commands are as follows:");
            Put_Line ("I : Start item editor.");
            Put_Line ("S : Save topic management file.");
            Put_Line ("T : Start topic (subscription) editor.");
            Put_Line ("Q : Quit the editor (unsaved changes will be lost).");
            Put_Line ("? : List commands.");
         when others =>
            Put_Line ("Invalid command");
      end case; -- Command
      Put ("Command [I | Q | S | T | ?]: ");
      Get_Immediate (Command);
      New_Line;
   end loop; -- Command /= 'q' and Command /= 'Q'
exception
   when E: others =>
      Put_Line ("Unhandled exceotion - " & Exception_Message (E));
end Topic_Editor;