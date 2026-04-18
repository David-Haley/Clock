--  This program is an interactive editor for IOT_Clock subscription files.
--  The primary reason for this program is to obfuscate the password of the
--  user by means of simple encryption.

--  Author    : David Haley
--  Created   : 18/04/2026
--  Last Edit : 19/04/2026

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with MQTT_Subscription; use MQTT_Subscription;

procedure Sub_Editor is

   Procedure Get_Susbcription (Topic, Broker, User, Password :
                               out Unbounded_String) is

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
      Put_Line ("D : Delete an existing topic.");
      Put_Line ("L : List stored topic information.");
      Put_Line ("M : Modify the information stored for an existing topic.");
      Put_Line ("P : Check password, compares stored password with user " &
                "entry.");
      Put_Line ("S : Save subscriptions to file.");
      Put_Line ("Q : Quit, caution unsaved subscription data will be lost.");
      Put_Line ("? : List commands.");
   end List_Commands;
   
   Answer, Command : Character;
   Topic, Broker, User, Password : Unbounded_String;
   
begin -- Sub_Editor
   Put_Line ("IOT_Clock Subscription Editor version 20260419");
   if File_Exists then
      Read_Subscription;
      Put_Line ("Subscription file opened");
      Command := '?';
   else
      Put ("Subscription file not found, create a new one [Y | N]: ");
      Get_Immediate (Answer);
      New_Line;
      if Answer = 'y' or Answer = 'Y' then
         Write_Subscription;
         Put_Line ("Subscription file created");
         Command := '?';
      else
         Command := 'Q';
      end if; -- Answer = 'y' or Answer = 'Y'
   end if; -- File_Exists
   while Command /= 'q' and Command /= 'Q' loop
      case Command is
         when 'a' | 'A' =>
            Put_Line ("Add a new topic.");
            Get_Susbcription (Topic, Broker, User, Password);
            if Topic_Exists (To_String (Topic)) then
               Put_Line ("Topic already exists, use Modify to change.");
            elsif Length (Topic) > 0 and Length (Broker) > 0 and
              Length (User) > 0
            then
               MQTT_Subscription.Create (To_String (Topic),
                                         To_String (Broker),
                                         To_String (User),
                                         To_String (Password));
            else
               Put_Line ("Zero length strings not permitted, nothing changed.");
            end if; -- Topic_Exists (To_String (Topic))
         when 'd' | 'D' =>
            Put_LIne ("Delete Topic, enter topic to be deleted.");
            Put ("Topic: ");
            Get_Line (Topic);
            if Topic_Exists (To_String (Topic)) then
               Delete (To_String (Topic));
            else
               Put_Line ("Topic not found, nothing changed.");
            end if; -- Topic_Exists (To_String (Topic))
         when 'l' | 'L' =>
            Put_Subscriptions;
         when 'm' | 'M' =>
            Put_Line ("Modify the information stored for a topic.");
            Put_Line ("Note: if Broker or User values are changed, Password" &
                      "must be updated");
            Put_Line ("Enter for no change or new value and enter to change.");
            Get_Susbcription (Topic, Broker, User, Password);
            if Topic_Exists (To_String (Topic)) then
               if Length (Broker) = 0 and Length (User) = 0 and
                 Length (Password) = 0
               then
                  Put_Line ("No values entered, nothing changed");
               else
                  Modify (To_String (Topic), To_String (Broker),
                          To_String (User), To_String (Password));
               end if; -- Length (Broker) = 0 and Length (User) = 0 and ...
            else
               Put_Line ("Topic not found, nothing changed.");
            end if; -- Topic_Exists (To_String (Topic))
         when 'p' | 'P' =>
            Put_Line ("Check Password");
            Put ("Topic: ");
            Get_Line (Topic);
            Put ("Password: ");
            Get_Line (Password);
            if Topic_Exists (To_String (Topic)) then
               if Get_Password (To_String (Topic)) = To_String (Password) then
                  Put_Line ("Passwords match");
               else
                  Put_Line ("Passwords do not match");
               end if; -- Get_Password (To_String (Topic)) = To_String (Password)
            else
               Put_Line ("Topic not found, password connot be checked.");
            end if; -- Topic_Exists (To_String (Topic))
         when 's' | 'S' =>
            Write_Subscription;
            Put_Line ("Subscription file written to disc");
         when '?' =>
            List_Commands;
         when others =>
            Put_Line ("Invalid command");
      end case; -- Command
      Put ("Command [A | D | L | M | P | S | Q | ?]: ");
      Get_Immediate (Command);
      New_Line;
   end loop; -- Command /= 'q' and Command /= 'Q'
end Sub_Editor;