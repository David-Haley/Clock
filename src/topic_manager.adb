--  This package manages the linkage between items displayable by the IOT clock
--  and subscribed topics. The concept of an MQTT_Item_Id is introduced that
--  maps to a specific value obtained from a subscribed topic and the
--  formatting operations that need to be applied. Note: only a single level
--  json file is suppotred, no nesting and no arrays.

--  Author    : David Haley
--  Created   : 24/04/2026
--  Last Edit : 24/05/2026

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories; use Ada.Directories;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Exceptions; use Ada.Exceptions;
with Interfaces; use Interfaces;
with GNATCOLL.JSON; use GNATCOLL.JSON;
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

   -- JSON Field_Names
   Item_Array_String : constant String := "Item_Array";
   Item_Id_String : constant String := "Item_Id";
   Topic_String : constant String := "Topic";
   Field_String : constant String := "Field";
   MQTT_Item_Type_String : constant String := "MQTT_Item_Type";
   Scaling_Factor_String : constant String := "Scaling_Factor";
   Decimal_Place_String : constant String := "Decimal_Place";
   Is_Signed_String : constant String := "Is_Signed";
   True_Text_String : constant String := "True_Text";
   False_Text_String : constant String := "False_Text";
   
   package Item_Stores is new 
     Ada.Containers.Indefinite_Ordered_Maps (MQTT_Item_Ids, Item_Records);
   use Item_Stores;

   package Sub_Stores is new 
     Ada.Containers.Indefinite_Ordered_Maps (Topics, Handle_Pointers);
   use Sub_Stores;

   Item_Store : Item_Stores.Map := Item_Stores.Empty_Map;
   Sub_Store : Sub_Stores.Map := Sub_Stores.Empty_Map;

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
      Item_Record.Topic := To_Unbounded_String (Topic);
      Item_Record.Field := To_Unbounded_String (Field);
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
      Item_Record.Topic := To_Unbounded_String (Topic);
      Item_Record.Field := To_Unbounded_String (Field);
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
      Item_Record.Topic := To_Unbounded_String (Topic);
      Item_Record.Field := To_Unbounded_String (Field);
      Item_Record.Scaling_Factor := Scaling_Factor;
      Item_Record.Decimal_Place := Decimal_Place;
      Item_Record.Is_Signed := Is_Signed;
      Insert (Item_Store, MQTT_Item_Id, Item_Record);
   end Create_Scaled_Number_Item;

   procedure Create_Boolean_Item (MQTT_Item_Id : in MQTT_Item_Ids;
                                  Topic : in Topics;
                                  Field : in Fields;
                                  True_Text : in String := "true";
                                  False_Text : in String := "false") is

      --  Creates a new Boolean_Item defining the topic and the field from which
      --  the Boolean is to be retrieved. True_Text and False_Text define the
      --  text to be displayed wnen the boolean is true and false respectively.
      --  For example when True display on and when false display off etc.

      Item_Record : Item_Records (MQTT_Boolean);

   begin -- Create_Boolean_Item
      if Item_Id_Exists (MQTT_Item_Id) then
         Delete_Item  (MQTT_Item_Id);
      end if; -- Item_Id_Exists (MQTT_Item_Id)
      Item_Record.Topic := To_Unbounded_String (Topic);
      Item_Record.Field := To_Unbounded_String (Field);
      Item_Record.True_Text := To_Unbounded_String (True_Text);
      Item_Record.False_Text := To_Unbounded_String (False_Text);
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
                              Sub_Store : in out Sub_Stores.Map)
                              return Boolean is

         --  Returns true if the MQTT_Item_Id is already supcribed. If not
         --  already subcribed attempts to set up a subscription and if
         --  successful returns true. It has the side effect of storing the
         --  MQTT_Handle if it subscribes sucessfully.

         Result : Boolean;

      begin -- Is_Subscribed
         Result := Item_Id_Exists (MQTT_Item_Id) and then
           Topic_Exists (To_String (Item_Store (MQTT_Item_Id).Topic));
         if Result then
            -- Check subscription
            declare -- Topic Declaration block
               Topic : constant Topics :=
                 To_String (Item_Store (MQTT_Item_Id).Topic);
            begin -- Topic Declaration block
               if not Contains (Sub_Store, Topic) then
                  declare -- Handle declaration block
                     Handle_Pointer : constant Handle_Pointers :=
                       new MQTT_Handle;
                  begin -- Handle declaration block
                     Connect_Rx (Get_Broker (Topic),
                                 Get_User (Topic),
                                 Get_Password (Topic),
                                 Topic,
                                 Handle_Pointer.all);
                     Insert (Sub_Store, Topic, Handle_Pointer);
                  end; -- Handle declaration block
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
      if Is_Subscribed (MQTT_Item_Id, Sub_Store) and then
        Is_Connected_Rx (Sub_Store (To_String (Item_Store (MQTT_Item_Id).Topic))
                         .all)
      then
         declare -- JSON_String block
            JSON_String : constant String :=
              Receive (Sub_Store (To_String (Item_Store (MQTT_Item_Id).Topic))
                .all);
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
      end if; -- Is_Subscribed (MQTT_Item_Id, Sub_Store)
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

   procedure Read_Topics is

      --  Reads in the an existing configuration file.

      Parsed : Read_Result;
      Item_Array : JSON_Array;
      Item_Type : MQTT_Item_Types;

   begin -- Read_Topics
      Parsed := Read_File (Topic_Management_File);
      if Parsed.Success then
         Item_Array := Get (Parsed.Value, Item_Array_String);
         Clear (Item_Store);
      else
         raise Topic_Error with "Read_Topics error Line:" &
           Parsed.Error.Line'Img & " Column:" &
           Parsed.Error.Column'Img & " Message : " &
           Format_Parsing_Error(Parsed.Error);
      end if;
      for I in Natural range 1 .. Length (Item_Array) loop
         Item_Type := MQTT_Item_Types'Value (Get (Get (Item_Array, I),
                                                  MQTT_Item_Type_String));
         declare -- Item_Record
            Item_Record : Item_Records (Item_Type);
            Item_Id : Unbounded_String;
         begin -- Item_Record
            Item_Id := Get (Get (Item_Array, I), Item_Id_String);
            Item_Record.Topic := Get (Get (Item_Array, I), Topic_String);
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
            Insert (Item_Store, To_String (Item_Id), Item_Record);
         end; -- Item_Record;
      end loop; --  I in Natural range 1 .. Length (Item_Array)
   exception
      when E: others => 
         raise Topic_Error with "Read_Topics - " & Exception_Message (E);
   end Read_Topics;

   procedure Write_Topics is

      --  Writes a new or over writes an existing configuration file.

      Global_JSON : constant JSON_Value := Create_Object;
      Item_Array : JSON_Array := Empty_Array;
      Output_File : File_Type;

   begin -- Write_Topics
      for I in Iterate (Item_Store) loop
         declare -- Item_JSON
            Item_JSON : constant JSON_Value := Create_Object; 
         begin -- Item_JSON
            Set_Field (Item_JSON, Item_Id_String, Key (I));
            Set_Field (Item_JSON, Topic_String, Create (Element (I).Topic));
            Set_Field (Item_JSON, Field_String, Create (Element (I).Field));
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
      end loop; -- I in Iterate (Item_Store)
      Set_Field (Global_JSON, Item_Array_String, Item_Array);
      Create (Output_File, Out_File, Topic_Management_File);
      Ada.Text_IO.Put (Output_File, Write (Global_JSON, False));
      Close (Output_File);
   exception
      when E: others => 
         raise Topic_Error with "Write_Subscription_Topics - " &
           Exception_Message (E);
   end Write_Topics;
   
end Topic_Manager;