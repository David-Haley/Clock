--  This package manages the topics that the IOT_Clock is subscribed to.
--  For each topic the associated broker (host name), user and password is
--  is stored and retrieved from a configuration file. The passwords are not
--  stored as plain text but have some simple encription applied.

--  Author    : David Haley
--  Created   : 17/04/2026
--  Last Edit : 19/04/2026

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Ordered_Maps;
with Ada.Exceptions; use Ada.Exceptions;
with DJH.Parse_CSV;
with DJH.One_Time; use DJH.One_Time;

package body MQTT_Subscription is

   type Header_Items is (Topic_H, Broker_H, User_H, Password_H);

   type Subscriptions is record
      Broker, User, Password : Unbounded_String := Null_Unbounded_String;
   end record; -- Subscriptions

   package Subscription_Stores is new
     Ada.Containers.Ordered_Maps (Unbounded_String, Subscriptions);
   use Subscription_Stores;

   Subscription_Store : Subscription_Stores.Map :=
     Subscription_Stores.Empty_Map;

   procedure Create (Topic : in Topics;
                     Broker : in Brokers;
                     User : in Users;
                     Password : in Passwords) is

      --  Creates a new entry or replaces an existing entry.

      Subscription : Subscriptions;

   begin -- Create
      Subscription := (To_Unbounded_String (Broker),
                       To_Unbounded_String (User),
                       To_Unbounded_String (Password));
      Insert (Subscription_Store, To_Unbounded_String (Topic), Subscription);
   exception
      when E: others =>
         raise Subscription_Error with "Create - " &
           Exception_Message (E);
   end Create;

   procedure Modify (Topic : in Topics;
                     Broker : in Brokers;
                     User : in Users;
                     Password : in Passwords) is

      --  Topic must already exist, allows changes to Broker, User and
      --  Password. Existing value is retained if an empty string is entered,
      --  otherwise the new value is saved.

      Subscription : Subscriptions;

   begin -- Modify
      Subscription := Subscription_Store (To_Unbounded_String(Topic));
      if Broker'Length > 0 then
         Subscription.Broker := To_Unbounded_String (Broker);
      end if; -- Broker'Length > 0
      if User'Length > 0 then
         Subscription.User := To_Unbounded_String (User);
      end if; -- User'Length > 0
      if Password'Length > 0 then
         Subscription.Password := To_Unbounded_String (Password);
      end if; -- Password'Length > 0
      Subscription_Store (To_Unbounded_String (Topic)) := Subscription;
   exception
      when E: others =>
         raise Subscription_Error with "Modify - " &
           Exception_Message (E);
   end Modify;


   procedure Delete (Topic : in Topics) is

      --  Deletes the specified topic.

   begin -- Delete
      Delete (Subscription_Store, To_Unbounded_String (Topic));
   exception
      when E: others =>
         raise Subscription_Error with "Delete - " &
           Exception_Message (E);
   end Delete;

   function Get_Broker (Topic : in Topics) return Brokers is

     --  Returns the broker's host name, from which to subscribe.

   begin -- Get_Broker
      return To_String (Subscription_Store
        (To_Unbounded_String(Topic)).Broker);
   exception
      when E: others =>
         raise Subscription_Error with "Get_Broker - " &
           Exception_Message (E);
   end Get_Broker;

   function Get_User (Topic : in Topics) return Users is

      --  Returns the user name to for login.

   begin -- Get_User
      return To_String (Subscription_Store
        (To_Unbounded_String (Topic)).User);
   exception
      when E: others =>
         raise Subscription_Error with "Get_User - " &
           Exception_Message (E);
   end Get_User;

   function Get_Password (Topic : in Topics) return Passwords is

      -- Returns the password associated with the user.

   begin -- Get_Password
      return To_String (Subscription_Store
        (To_Unbounded_String (Topic)).Password);
   exception
      when E: others =>
         raise Subscription_Error with "Get_Password - " &
           Exception_Message (E);
   end Get_Password;

   --  Note all Get functions raise a Subscription_Error exception if a
   --  there is not subscription recorded for that topic.

   function File_Exists return Boolean is

   --  Returns true if the subscription configuration exixts;
   
      (Exists (MQTT_Subscription_File) and then
        Kind (MQTT_Subscription_File) = Ordinary_File);

   function Topic_Exists (Topic : in Topics) return Boolean is

   --  Returns true if a subscription exists for the topic.

      (Contains (Subscription_Store, To_Unbounded_String(Topic)));

   function Make_Key (Broker : in Brokers;
                      User : in Users) return String is

      Result : Unbounded_String := Null_Unbounded_String;
      I : Positive := 1;

   begin -- Make_Key
      while I <= Broker'Length or I <= User'Length loop
         if I <= Broker'Length then
            Result := @ & Broker (I);
         end if; -- I <= Broker'Length
         if I <= User'Length then
            Result := @ & User (I);
         end if; -- I <= User'Length
         I := @ + 1;
      end loop; -- I <= Broker'Length or I <= User'Length
      return To_String (Result);
   end Make_Key;

   procedure Read_Subscription is

   --  Reads an existing subscription file, raise a Subscription_Error
   --  exception if the file cannot be read or errors occur whilst reading
   --  the file.

      package Parser is new DJH.Parse_CSV (Header_Items);
      use Parser;

   begin -- Read_Subscription
      Read_Header (MQTT_Subscription_File);
      while Next_Row loop
         declare -- Subscription declaration block

            Topic : Topics := Get_Value (Topic_H);
            Subscription : Subscriptions;

         begin -- Subscription declaration block
            Subscription.Broker := To_Unbounded_String (Get_Value (Broker_H));
            Subscription.User := To_Unbounded_String (Get_Value (User_H));
            Subscription.Password :=
              To_Unbounded_String (Decode (Get_Value (Password_H),
                Make_Key (Get_Value (Broker_H), Get_Value (User_H))));
            insert (Subscription_Store, To_Unbounded_String (Topic),
                    Subscription);
         end; -- Subscription declaration block
      end loop; -- Next_Row
      Close_CSV;
   exception
      when E: others =>
         raise Subscription_Error with "Read_Subscriptions at row" &
           Row_Number'Img & " - " & Exception_Message (E);
   end Read_Subscription;

   procedure Write_Subscription is

   --  Writes (or overwrites) a subscription file, raise a Subscription_Error
   --  exception if the file is not written successfully.

      Delimiter : constant Character := ',';

      Output_File : File_Type;

   begin -- Write_Subscription
      Create (Output_File, Out_File, MQTT_Subscription_File);
      for H in Header_Items loop
         Put (Output_File, H'Img);
         if H /= Header_Items'Last then
            Put (Output_File, Delimiter);
         else
            New_Line (Output_File);
         end if; -- H in Header_Items
      end loop; -- H in Header_Items
      for T in Iterate (Subscription_Store) loop
         Put_Line (Output_File, To_String (Key (T)) & Delimiter &
                   To_String (Element (T).Broker) & Delimiter &
                   To_String (Element (T).User) & Delimiter &
                   Encode (To_String (Element (T).Password),
                           Make_Key (To_String (Element (T).Broker),
                                     To_String (Element (T).User))));
      end loop; -- T in Iterate (Subscription_Store)
      Close (Output_File);
   exception
      when E: others =>
         raise Subscription_Error with "Write_Subscription - " &
           Exception_Message (E);
   end Write_Subscription;

   procedure Put_Subscriptions is

      --  Lists subscriptions to standard output.

      Delimiter : constant Character := ' ';

   begin -- Put_Subscriptions
      Put_Line ("List of Topics and Broker details");
      for T in Iterate (Subscription_Store) loop
         Put (To_String (Key (T)) & Delimiter &
              To_String (Element (T).Broker) & Delimiter &
              To_String (Element (T).User) & Delimiter);
            if Length (Element(T).Password) > 0 then
               Put_Line ("<Has Password>");
            else
               Put_Line ("<No Password>");
            end if;
      end loop; -- T in Iterate (Subscription_Store)
   end Put_Subscriptions;

end MQTT_Subscription;