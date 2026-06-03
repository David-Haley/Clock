--  This package manages the linkage between items displayable by the IOT clock
--  and subscribed topics. The concept of an MQTT_Item_Id is introduced that
--  maps to a specific value obtained from a subscribed topic and the
--  formatting operations that need to be applied. Note: only a single level
--  json file is suppotred, no nesting and no arrays.

--  Author    : David Haley
--  Created   : 24/04/2026
--  Last Edit : 02/06/2026

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Directories; use Ada.Directories;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Exceptions; use Ada.Exceptions;
with Interfaces; use Interfaces;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with DJH.One_Time; use DJH.One_Time;
with MQTT_Client; use MQTT_Client;

package body Topic_Manager is

   type Item_Records (MQTT_Item_Type : MQTT_Item_Types) is record
      Topic, Field : Unbounded_String;
      case MQTT_Item_Type is
         when MQTT_String | MQTT_Number =>
            null;
         when MQTT_U16 | MQTT_U32 =>
            Scaling_Factor : Scaling_Factors := 1;
            Decimal_Place : Decimal_Places := 0;
            Is_Signed : Boolean := False;
         when MQTT_Boolean =>
            True_Text : Unbounded_String := To_Unbounded_String ("true");
            False_Text :Unbounded_String := To_Unbounded_String ("false");
      end case; -- MQTT_Item_Type
   end record; -- Item_Records

   type Handle_Pointers is access MQTT_Handle;

   type Subscriptions is record
      Broker : Brokers := Null_Unbounded_String;
      User : Users := Null_Unbounded_String;
      Password : Passwords := Null_Unbounded_String;
      Handle_Pointer : Handle_Pointers := null;
   end record; -- Subscriptions
   
   package Item_Stores is new 
     Ada.Containers.Indefinite_Ordered_Maps (MQTT_Item_Ids, Item_Records);
   use Item_Stores;

   package Subscription_Stores is new
     Ada.Containers.Ordered_Maps (Topics, Subscriptions);
   use Subscription_Stores;

   -- Package global data 

   Subscription_Store : Subscription_Stores.Map :=
     Subscription_Stores.Empty_Map;
   Item_Store : Item_Stores.Map := Item_Stores.Empty_Map;

   --  Subscription management procedures and functions

   procedure Create_Subscription (Topic : in Topics;
                                  Broker : in Brokers;
                                  User : in Users;
                                  Password : in Passwords) is

      --  Creates a new entry or replaces an existing entry.

      Subscription : Subscriptions;

   begin -- Create_Subscription
      Subscription := (Broker, User, Password, null);
      Insert (Subscription_Store, Topic, Subscription);
   exception
      when E: others =>
         raise Subscription_Error with "Create - " &
           Exception_Message (E);
   end Create_Subscription;

   procedure Modify_Subscription (Topic : in Topics;
                     Broker : in Brokers;
                     User : in Users;
                     Password : in Passwords) is

      --  Topic must already exist, allows changes to Broker, User and
      --  Password. Existing value is retained if an empty string is entered,
      --  otherwise the new value is saved.

      Subscription : Subscriptions;

   begin -- Modify_Subscription
      Subscription := Subscription_Store (Topic);
      if Length (Broker) > 0 then
         Subscription.Broker := Broker;
      end if; -- Length (Broker) > 0
      if Length (User) > 0 then
         Subscription.User := User;
      end if; -- Length (User) > 0
      if Length (Password) > 0 then
         Subscription.Password := Password;
      end if; -- Length (Password) > 0
      Subscription_Store (Topic) := Subscription;
   exception
      when E: others =>
         raise Subscription_Error with "Modify - " &
           Exception_Message (E);
   end Modify_Subscription;

   procedure Delete_Subscription (Topic : in Topics) is

      --  Deletes the specified topic.

   begin -- Delete_Subscription
      Delete (Subscription_Store, Topic);
   exception
      when E: others =>
         raise Subscription_Error with "Delete - " &
           Exception_Message (E);
   end Delete_Subscription;

   --  Note all Get functions raise a Subscription_Error exception if a
   --  there is no subscription recorded for that topic.

   function Get_Broker (Topic : in Topics) return Brokers is

      --  Returns the broker's host name, from which to subscribe.

   begin -- Get_Broker
      return Subscription_Store (Topic).Broker;
   exception
      when E: others =>
         raise Subscription_Error with "Get_Broker - " &
           Exception_Message (E);
   end Get_Broker;

   function Get_User (Topic : in Topics) return Users is

      --  Returns the user name to for login.

   begin -- Get_User
      return Subscription_Store (Topic).User;
   exception
      when E: others =>
         raise Subscription_Error with "Get_User - " &
           Exception_Message (E);
   end Get_User;

   function Get_Password (Topic : in Topics) return Passwords is

      -- Returns the password associated with the user.

   begin -- Get_Password
      return Subscription_Store (Topic).Password;
   exception
      when E: others =>
         raise Subscription_Error with "Get_Password - " &
           Exception_Message (E);
   end Get_Password;

   function Topic_Exists (Topic : in Topics) return Boolean is

   --  Returns true if a subscription exists for the topic.

      (Contains (Subscription_Store, Topic));

   procedure Put_Subscriptions is

      --  Lists subscriptions to standard output.

      Delimiter : constant Character := ' ';

   begin -- Put_Subscriptions
      Put_Line ("List of Topics and Broker details");
      for T in Iterate (Subscription_Store) loop
         Put (Key (T) & Delimiter &
              Element (T).Broker & Delimiter &
              Element (T).User & Delimiter);
            if Length (Element(T).Password) > 0 then
               Put_Line ("<Has Password>");
            else
               Put_Line ("<No Password>");
            end if;
      end loop; -- T in Iterate (Subscription_Store)
   end Put_Subscriptions;

   --  The create procedures below will overite stored information for the
   --  relevant item if it already exists  

   procedure Create_String_Item (MQTT_Item_Id : in MQTT_Item_Ids;
                                 Topic : in Topics;
                                 Field : Fields) is

      --  Creates a new String_Item defining the topic and the field from which
      --  the string is to be retrieved.

      Item_Record : Item_Records (MQTT_String);

   begin -- Create_String_Item
      if Item_Id_Exists (MQTT_Item_Id) then
         Delete_Item  (MQTT_Item_Id);
      end if; -- Item_Id_Exists (MQTT_Item_Id)
      Item_Record.Topic := Topic;
      Item_Record.Field := Field;
      Insert (Item_Store, MQTT_Item_Id, Item_Record);
   end Create_String_Item;

   procedure Create_Number_Item (MQTT_Item_Id : in MQTT_Item_Ids;
                                 Topic : in Topics;
                                 Field : in Fields) is

      --  Creates a new Number_Item defining the topic and the field from which
      --  the number is to be retrieved. The assumption is that the number is
      --  directly capable of being displayed.

      Item_Record : Item_Records (MQTT_Number);

   begin -- Create_Number_Item
      if Item_Id_Exists (MQTT_Item_Id) then
         Delete_Item  (MQTT_Item_Id);
      end if; -- Item_Id_Exists (MQTT_Item_Id)
      Item_Record.Topic := Topic;
      Item_Record.Field := Field;
      Insert (Item_Store, MQTT_Item_Id, Item_Record);
   end Create_Number_Item;

   procedure Create_Scaled_Number_Item (MQTT_Item_Id : in MQTT_Item_Ids;
                                        Topic : in Topics;
                                        Field : in Fields;
                                        Scaled_Number : in Scaled_Numbers;
                                        Scaling_Factor : in Scaling_Factors
                                          := 1;
                                        Decimal_Place : in Decimal_Places := 0;
                                        Is_Signed : in Boolean := False) is

      --  Creates a new Scaled_Number_Item defining the topic and the field from
      --  which the number is to be retrieved. This is intended to be used to
      --  take a value that may have come more or less directly from a source
      --  such as a modbus register and display it scaled to real world units.
      --  the source value read in can be treated as a signed value (twos
      --  complement), converted to an integer and by scaling and controling
      --  the number of decimal places converted to a fixed point
      --  representation.

      Item_Record : Item_Records (Scaled_Number);

   begin -- Create_Scaled_Number_Item
      if Item_Id_Exists (MQTT_Item_Id) then
         Delete_Item  (MQTT_Item_Id);
      end if; -- Item_Id_Exists (MQTT_Item_Id)
      Item_Record.Topic := Topic;
      Item_Record.Field := Field;
      Item_Record.Scaling_Factor := Scaling_Factor;
      Item_Record.Decimal_Place := Decimal_Place;
      Item_Record.Is_Signed := Is_Signed;
      Insert (Item_Store, MQTT_Item_Id, Item_Record);
   end Create_Scaled_Number_Item;

   procedure Create_Boolean_Item (MQTT_Item_Id : in MQTT_Item_Ids;
                                  Topic : in Topics;
                                  Field : in Fields;
                                  True_Text : in Unbounded_String :=
                                   To_Unbounded_String ("true");
                                  False_Text : in Unbounded_String :=
                                   To_Unbounded_String ("false")) is

      --  Creates a new Boolean_Item defining the topic and the field from which
      --  the Boolean is to be retrieved. True_Text and False_Text define the
      --  text to be displayed wnen the boolean is true and false respectively.
      --  For example when True display on and when false display off etc.

      Item_Record : Item_Records (MQTT_Boolean);

   begin -- Create_Boolean_Item
      if Item_Id_Exists (MQTT_Item_Id) then
         Delete_Item  (MQTT_Item_Id);
      end if; -- Item_Id_Exists (MQTT_Item_Id)
      Item_Record.Topic := Topic;
      Item_Record.Field := Field;
      Item_Record.True_Text := True_Text;
      Item_Record.False_Text := False_Text;
      Insert (Item_Store, MQTT_Item_Id, Item_Record);
   end Create_Boolean_Item;

   procedure Delete_Item (MQTT_Item_Id : in MQTT_Item_Ids) is

      -- Deletes an item of any type.

   begin -- Delete_Item
      if Item_Id_Exists (MQTT_Item_Id) then
         Delete  (Item_Store, MQTT_Item_Id);
      else
         raise Topic_Error with "Delete_Item, topic id """ & MQTT_Item_Id &
           """ does not exist.";
      end if; -- Item_Id_Exists (MQTT_Item_Id)
   end Delete_Item;

   function Get_For_Display (MQTT_Item_Id : in MQTT_Item_Ids) return String is

      --  Gets the text to be displayed based on the most recent value obtained
      --  from the subscribed topic

      type Fixed_0 is delta 1.0 digits 14;
      type Fixed_1 is delta 0.1 digits 14;
      type Fixed_2 is delta 0.01 digits 14;
      type Fixed_3 is delta 0.001 digits 14;

      Error_01 : constant String := "Emq 01"; -- Subscription failed
      Error_02 : constant String := "Emq 02"; -- Not a number, raw no formatting
      Error_03 : constant String := "Emq 03"; -- Not a string
      Error_04 : constant String := "Emq 04"; -- Not a number, U16 formatting
      Error_05 : constant String := "Emq 05"; -- Not a number, U32 formatting
      Error_06 : constant String := "Emq 06"; -- Not a Boolean
      Error_07 : constant String := "Emq 07"; -- Parsing failure

      function Is_Subscribed (MQTT_Item_Id : in MQTT_Item_Ids;
                              Subscription_Store :
                                in out Subscription_Stores.Map)
                              return Boolean is

         --  Returns true if the MQTT_Item_Id is already supcribed. If not
         --  already subcribed attempts to set up a subscription and if
         --  successful returns true. It has the side effect of storing the
         --  MQTT_Handle if it subscribes sucessfully.

         Result : Boolean;

      begin -- Is_Subscribed
         Result := Item_Id_Exists (MQTT_Item_Id) and then
           Topic_Exists (Item_Store (MQTT_Item_Id).Topic);
         if Result then
            -- Check subscription
            declare -- Topic Declaration block
               Topic : constant Topics := Item_Store (MQTT_Item_Id).Topic;
            begin -- Topic Declaration block
               if Subscription_Store (Topic).Handle_Pointer = null then
                  Subscription_Store (Topic).Handle_Pointer := new MQTT_Handle;
                  Connect_Rx (To_String (Get_Broker (Topic)),
                              To_String (Get_User (Topic)),
                              To_String (Get_Password (Topic)),
                              To_String (Topic),
                              Subscription_Store (Topic).Handle_Pointer.all);
               end if; -- not Contains (Sub_Store, Topic)
            end; -- Topic Declaration block
         end if; -- not Result
         return Result;
      exception
         when others =>
            return False;
      end Is_Subscribed;

      function Number_Item (Value : in JSON_Value) return String is
         
         Number_I : Integer;
         Number_F : Float;

      begin -- Number_Item
         case Kind (Value) is
            when JSON_Int_Type =>
               Number_I := Get (Value);
               return Trim (Number_I'Img, Both);
            when JSON_Float_Type =>
               Number_F := Get (Value);
               return Trim (Number_F'Img, Both);
            when others =>
               return Error_02;
         end case; -- Kind (Value)
      end; -- Number_Item

      function String_Item (Value : in JSON_Value) return String is

      begin -- String_Item
         if Kind (Value) = JSON_String_Type then
            return Get (Value);
         else
            return Error_03;
         end if; -- Kind (Value) = JSON_String_Type
      end; -- String_Item

      function U16_Item (Item_Store : in Item_Stores.Map;
                         MQTT_Item_Id : in MQTT_Item_Ids;
                         Value : in JSON_Value) return String is

         Sign_Bit : constant Unsigned_16 := 16#8000#;
         Number : Integer;
         U_16 : Unsigned_16;
         N_0 : Fixed_0;
         N_1 : Fixed_1;
         N_2 : Fixed_2;
         N_3 : Fixed_3;

      begin -- U16_Item
         if Kind (Value) = JSON_Int_Type then
            Number := Get (Value);
            U_16 := Unsigned_16 (Number);
            case Item_Store (MQTT_Item_Id).Decimal_Place is
               when 0 =>
               if Item_Store (MQTT_Item_Id).Is_Signed and then
                 (U_16 and Sign_Bit) /= 0
               then
                  N_0 := Fixed_0 ((not U_16) - 1);
               else
                  N_0 := Fixed_0 (U_16);
               end if; -- Iten_Store (MQTT_Item_Id).Is_Signed ...
               N_0 := N_0 / Fixed_0 (Item_Store (MQTT_Item_Id).Scaling_Factor); 
               return Trim (N_0'Img, Both);
               when 1 =>
               if Item_Store (MQTT_Item_Id).Is_Signed and then
                 (U_16 and Sign_Bit) /= 0
               then
                  N_1 := Fixed_1 ((not U_16) - 1);
               else
                  N_1 := Fixed_1 (U_16);
               end if; -- Item_Store (MQTT_Item_Id).Is_Signed ...
               N_1 := N_1 / Fixed_1 (Item_Store (MQTT_Item_Id).Scaling_Factor); 
               return Trim (N_1'Img, Both);
               when 2 =>
               if Item_Store (MQTT_Item_Id).Is_Signed and then
                 (U_16 and Sign_Bit) /= 0
               then
                  N_2 := Fixed_2 ((not U_16) - 1);
               else
                  N_2 := Fixed_2 (U_16);
               end if; -- Item_Store (MQTT_Item_Id).Is_Signed ...
               N_2 := N_2 / Fixed_2 (Item_Store (MQTT_Item_Id).Scaling_Factor); 
               return Trim (N_2'Img, Both);
               when 3 =>
               if Item_Store (MQTT_Item_Id).Is_Signed and then
                 (U_16 and Sign_Bit) /= 0
               then
                  N_3 := Fixed_3 ((not U_16) - 1);
               else
                  N_3 := Fixed_3 (U_16);
               end if; -- Item_Store (MQTT_Item_Id).Is_Signed ...
               N_3 := N_3 / Fixed_3 (Item_Store (MQTT_Item_Id).Scaling_Factor); 
               return Trim (N_3'Img, Both);
            end case; -- Item_Store (MQTT_Item_Id).Decimal_Place 
         else
            return Error_04;
         end if; -- Kind (Value)
      end U16_Item;

      function U32_Item (Item_Store : in Item_Stores.Map;
                         MQTT_Item_Id : in MQTT_Item_Ids;
                         Value : in JSON_Value) return String is


         Sign_Bit : constant Unsigned_32 := 16#8000_0000#;
         Number : Long_Integer;
         U_32 : Unsigned_32;
         N_0 : Fixed_0;
         N_1 : Fixed_1;
         N_2 : Fixed_2;
         N_3 : Fixed_3;

      begin -- U32_Item
         if Kind (Value) = JSON_Int_Type then
            Number := Get (Value);
            U_32 := Unsigned_32 (Number);
            case Item_Store (MQTT_Item_Id).Decimal_Place is
               when 0 =>
               if Item_Store (MQTT_Item_Id).Is_Signed and then
                 (U_32 and Sign_Bit) /= 0
               then
                  N_0:= Fixed_0 ((not U_32) - 1);
               else
                  N_0:= Fixed_0 (U_32);
               end if; -- Item_Store (MQTT_Item_Id).Is_Signed ...
               N_0 := N_0 / Fixed_0 (Item_Store (MQTT_Item_Id).Scaling_Factor); 
               return Trim (N_0'Img, Both);
               when 1 =>
               if Item_Store (MQTT_Item_Id).Is_Signed and then
                 (U_32 and Sign_Bit) /= 0
               then
                  N_1 := Fixed_1 ((not U_32) - 1);
               else
                  N_1 := Fixed_1 (U_32);
               end if; -- Item_Store (MQTT_Item_Id).Is_Signed ...
               N_1 := N_1 / Fixed_1 (Item_Store (MQTT_Item_Id).Scaling_Factor); 
               return Trim (N_1'Img, Both);
               when 2 =>
               if Item_Store (MQTT_Item_Id).Is_Signed and then
                 (U_32 and Sign_Bit) /= 0
               then
                  N_2 := Fixed_2 ((not U_32) - 1);
               else
                  N_2 := Fixed_2 (U_32);
               end if; -- Item_Store (MQTT_Item_Id).Is_Signed ...
               N_2 := N_2 / Fixed_2 (Item_Store (MQTT_Item_Id).Scaling_Factor); 
               return Trim (N_2'Img, Both);
               when 3 =>
               if Item_Store (MQTT_Item_Id).Is_Signed and then
                 (U_32 and Sign_Bit) /= 0
               then
                  N_3 := Fixed_3 ((not U_32) - 1);
               else
                  N_3 := Fixed_3 (U_32);
               end if; -- Item_Store (MQTT_Item_Id).Is_Signed ...
               N_3 := N_3 / Fixed_3 (Item_Store (MQTT_Item_Id).Scaling_Factor); 
               return Trim (N_3'Img, Both);
            end case; -- Item_Store (MQTT_Item_Id).Decimal_Place 
         else
            return Error_05;
         end if; -- Kind (Value)
      end U32_Item;

      function Boolean_Item (Item_Store : in Item_Stores.Map;
                             MQTT_Item_Id : in MQTT_Item_Ids;
                             Value : in JSON_Value) return String is

      begin -- Boolean_Item
         if Kind (Value) = JSON_Boolean_Type then
            if Get (Value) then
               return To_String (Item_Store (MQTT_Item_Id).True_Text);
            else
               return To_String (Item_Store (MQTT_Item_Id).False_Text);
            end if; -- Get (Value)
         else
            return Error_06;
         end if; -- Kind (Value) = JSON_Boolean_Type
      end Boolean_Item;

   begin -- Get_For_Display
      if Is_Subscribed (MQTT_Item_Id, Subscription_Store) and then
        Is_Connected_Rx (Subscription_Store (Item_Store (MQTT_Item_Id).Topic).
                         Handle_Pointer.all)
      then
         declare -- JSON_String block
            JSON_String : constant String :=
              Receive (Subscription_Store (Item_Store (MQTT_Item_Id).Topic).
                       Handle_Pointer.all);
            Parsed : Read_Result;
            Field : constant String :=
              To_String (Item_Store (MQTT_Item_Id).Field);
         begin -- JSON_String block
            Parsed := Read (JSON_String);
            if Parsed.Success and then Has_Field (Parsed.Value, Field) then
               case Item_Type (MQTT_Item_Id) is
                  when MQTT_Number =>
                     return Number_Item (Get (Parsed.Value, Field));
                  when MQTT_String =>
                     return String_Item (Get (Parsed.Value, Field));
                  when MQTT_U16 =>
                     return U16_Item (Item_Store,
                                      MQTT_Item_Id,
                                      Get (Parsed.Value, Field));
                  when MQTT_U32 =>
                     return U32_Item (Item_Store,
                                      MQTT_Item_Id,
                                      Get (Parsed.Value, Field));
                  when MQTT_Boolean =>
                     return Boolean_Item (Item_Store,
                                          MQTT_Item_Id,
                                          Get (Parsed.Value, Field));
               end case; -- Item_Type (MQTT_Item_Id)
            else
               return Error_07;
            end if; -- Parsed.Success and then Has_Field (Parsed.Value, Field)
         end; -- JSON_String block
      else
         return Error_01;
      end if; -- Is_Subscribed (MQTT_Item_Id, Subscription_Store)
   end Get_For_Display;

   procedure List_Items is

      --  Lists all item Id, with the topic and field, one per line.

   begin -- List_Items
      for I in Iterate (Item_Store) loop
         Put_Line (Key (I) & " : " & Item_Store (I).Topic & " " &
                   Item_Store (I).Field);
      end loop; -- I in Iterate (Item_Store)
   end List_Items;

   procedure Put_Item (MQTT_Item_Id : in MQTT_Item_Ids) is

      --  Uses the 'Image attribute to display an Item Id's data

   begin -- Put_Item
      if Item_Id_Exists (MQTT_Item_Id) then
         Put_Line (Item_Records'Image (Item_Store (MQTT_Item_Id)));
      else
         raise Topic_Error with "Put_Item, topic id """ & MQTT_Item_Id &
           """ does not exist.";
      end if; -- Item_Id_Exists (MQTT_Item_Id)
   end Put_Item;

   function Item_Id_Exists (MQTT_Item_Id : in MQTT_Item_Ids) return Boolean is

      --  Returns True if Item_Id has been defined.
      (Contains (Item_Store, MQTT_Item_Id));

   function Item_Type (MQTT_Item_Id : in MQTT_Item_Ids)
                       return MQTT_Item_Types is

      --  Returns the type of item represented by Item_Id

   begin -- Item_Type
      if not Item_Id_Exists (MQTT_Item_Id) then
         raise Topic_Error with "Item_Type, topic id """ & MQTT_Item_Id &
           """ does not exist.";
      end if; -- not Item_Id_Exists (MQTT_Item_Id)
      return Item_Store (MQTT_Item_Id).MQTT_Item_Type;
   end Item_Type;

   function File_Exists return Boolean is 

      --  Returns true if the topic file exists and is an ordinary file.
   
      (Exists (Topic_Management_File) and then
        Kind (Topic_Management_File) = Ordinary_File);

   -- JSON Field_Names
   Broker_String : constant String := "Broker";
   User_String : constant String := "User";
   Topic_String : constant String := "Topic";
   Topic_Array_String : constant String := "Topic_Array";
   Password_String : constant String := "Password";
   Item_Array_String : constant String := "Item_Array";
   Item_Id_String : constant String := "Item_Id";
   Field_String : constant String := "Field";
   MQTT_Item_Type_String : constant String := "MQTT_Item_Type";
   Scaling_Factor_String : constant String := "Scaling_Factor";
   Decimal_Place_String : constant String := "Decimal_Place";
   Is_Signed_String : constant String := "Is_Signed";
   True_Text_String : constant String := "True_Text";
   False_Text_String : constant String := "False_Text";

   function Make_Key (Broker : in Brokers;
                      User : in Users) return String is

      Result : Unbounded_String := Null_Unbounded_String;
      I : Positive := 1;

   begin -- Make_Key(
      while I <= Length (Broker) or I <= Length (User) loop
         if I <= Length (Broker) then
            Result := @ & Element (Broker, I);
         end if; -- I <= Length (Broker)
         if I <= Length (User) then
            Result := @ & Element (User, I);
         end if; -- I <= Length (User)
         I := @ + 1;
      end loop; -- I <= Length (Broker) or I <= Length (User)
      return To_String (Result);
   end Make_Key;

   procedure Read_Topics is

      --  Reads in the an existing configuration file.

      function Decode (Broker : in Brokers;
                       User : in Users;
                       Password_Array : in JSON_Array) return Passwords is

         Char_Int : Integer;
         Encoded_Password : Passwords := Null_Unbounded_String;

      begin -- Decode
         for I in Natural range 1 .. Length (Password_Array) loop
            Char_Int := Get (Get (Password_Array, I));
            Encoded_Password := @ & Character'Val (Char_Int);
         end loop; -- I in Natural range 1 .. Length (Password_Array)
         return To_Unbounded_String (Decode (To_String (Encoded_Password),
           Make_Key (Broker, User)));
      end Decode;

      Parsed : Read_Result;
      Topic_Array, Item_Array, Password_Array : JSON_Array;
      Item_Type : MQTT_Item_Types;
      Topic : Topics;
      Subscription : Subscriptions;

   begin -- Read_Topics
      Parsed := Read_File (Topic_Management_File);
      if Parsed.Success then
         Topic_Array := Get (Parsed.Value, Topic_Array_String);
         Clear (Subscription_Store);
         Clear (Item_Store);
      else
         raise Topic_Error with "Read_Topics error Line:" &
           Parsed.Error.Line'Img & " Column:" &
           Parsed.Error.Column'Img & " Message : " &
           Format_Parsing_Error(Parsed.Error);
      end if;
      for T in Natural range 1 .. Length (Topic_Array) loop
         Subscription.Broker := Get (Get (Topic_Array, T), Broker_String);
         Subscription.User := Get (Get (Topic_Array, T), User_String);
         Password_Array := Get (Get (Topic_Array, T), Password_String);
         Subscription.Password :=
           Decode (Subscription.Broker, Subscription.User, Password_Array);
         Topic := Get (Get (Topic_Array, T), Topic_String);
         Item_Array := Get (Get (Topic_Array, T), Item_Array_String);
         Insert (Subscription_Store, Topic, Subscription);
         for I in Natural range 1 .. Length (Item_Array) loop
            Item_Type := MQTT_Item_Types'Value (Get (Get (Item_Array, I),
                                                   MQTT_Item_Type_String));
            declare -- Item_Record
               Item_Record : Item_Records (Item_Type);
               Item_Id : constant MQTT_Item_Ids :=
                 Get (Get (Item_Array, I), Item_Id_String);
            begin -- Item_Record
               Item_Record.Topic := Topic;
               Item_Record.Field := Get (Get (Item_Array, I), Field_String);
               case Item_Type is
                  when MQTT_String | MQTT_Number =>
                     null;
                  when  MQTT_U16 | MQTT_U32 =>
                     Item_Record.Scaling_Factor :=
                     Get (Get (Item_Array, I), Scaling_Factor_String);
                     Item_Record.Decimal_Place :=
                     Get (Get (Item_Array, I), Decimal_Place_String);
                     Item_Record.Is_Signed :=
                     Get (Get (Item_Array, I), Is_Signed_String);
                  when MQTT_Boolean =>
                     Item_Record.True_Text :=
                     Get (Get (Item_Array, I), True_Text_String);
                     Item_Record.False_Text :=
                     Get (Get (Item_Array, I), False_Text_String);
               end case; -- Item_Type
               Insert (Item_Store, Item_Id, Item_Record);
            end; -- Item_Record;
         end loop; --  I in Natural range 1 .. Length (Item_Array)
      end loop; -- T in Natural range 1 .. Length (Topic_Array)
   exception
      when E: others => 
         raise Topic_Error with "Read_Topics - " & Exception_Message (E);
   end Read_Topics;

   procedure Write_Topics is 

      --  Writes a new or over writes an existing configuration file.

      function Encode (Broker : in Brokers;
                       User : in Users;
                       Password : in Passwords) return JSON_Array is
         
         Key : constant String := Make_Key (Broker, User);
         Encoded_Password : constant String :=
           Encode (To_String (Password), Key);
         Result : JSON_Array := Empty_Array;

      begin -- Encode
         for I in Positive range 1 .. Encoded_Password'Length loop
            Append (Result,
                    Create (Integer (Character'Pos (Encoded_Password (I)))));
         end loop; -- I in Positive range 1 .. Encoded_Password'Length
         return Result;
      end Encode;

      Global_JSON : constant JSON_Value := Create_Object;
      Topic_Array, Item_Array : JSON_Array;
      Output_File : File_Type;
      Topic_JSON : JSON_Value;

   begin -- Write_Topics
      Topic_Array := Empty_Array;
      for T in Iterate (Subscription_Store) loop
         Topic_JSON := Create_Object;
         Set_Field (Topic_JSON, Broker_String, Element (T).Broker);
         Set_Field (Topic_JSON, User_String, Element (T).User);
         Set_Field (Topic_JSON, Topic_String, Key (T));
         Set_Field (Topic_JSON, Password_String,
                    Encode (Element (T).Broker, Element (T).User,
                            Element (T).Password));
         Item_Array := Empty_Array;
         for I in Iterate (Item_Store) loop
            if Key (T) = Element(I).Topic then
               declare -- Item_JSON
                  Item_JSON : constant JSON_Value := Create_Object; 
               begin -- Item_JSON
                  Set_Field (Item_JSON, Item_Id_String, Key (I));
                  Set_Field (Item_JSON, Field_String,
                             Create (Element (I).Field));
                  Set_Field (Item_JSON, MQTT_Item_Type_String,
                  Create (Element (I).MQTT_Item_Type'Img));
                  case Element (I).MQTT_Item_Type is
                     when MQTT_String | MQTT_Number =>
                        null;
                     when  MQTT_U16 | MQTT_U32 =>
                        Set_Field (Item_JSON, Scaling_Factor_String,
                        Create (Element (I).Scaling_Factor));
                        Set_Field (Item_JSON, Decimal_Place_String,
                        Create (Element (I).Decimal_Place));
                        Set_Field (Item_JSON, Is_Signed_String,
                        Create (Element (I).Is_Signed));
                     when MQTT_Boolean =>
                        Set_Field (Item_JSON, True_Text_String,
                           Create (Element (I).True_Text));
                        Set_Field (Item_JSON, False_Text_String,
                           Create (Element (I).False_Text));
                  end case; -- Element (I).MQTT_Item_Type
                  Append (Item_Array, Clone (Item_JSON));
               end; -- Item_JSON
               Set_Field (Topic_JSON, Item_Array_String, Item_Array);
            end if; -- Key (T) = Element(I).Topic
         end loop; -- I in Iterate (Item_Store)
         Append (Topic_Array, Clone (Topic_JSON));
      end loop; -- T in Iterate (Subscription_Store)
      Set_Field (Global_JSON, Topic_Array_String, Topic_Array);
      Create (Output_File, Out_File, Topic_Management_File);
      Ada.Text_IO.Put (Output_File, Write (Global_JSON, False));
      Close (Output_File);
   exception
      when E: others => 
         raise Topic_Error with "Write_Subscription_Topics - " &
           Exception_Message (E);
   end Write_Topics;
   
end Topic_Manager;