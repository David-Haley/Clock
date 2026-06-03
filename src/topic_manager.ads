--  This package manages the topics that the IOT_Clock is subscribed to.
--  For each topic the associated broker (host name), user and password is
--  is stored and retrieved from the Topic_Management_File file. The passwords
--  are stored with simple encription applied. The package also manages the
--  linkage between items displayable by the IOT clock and subscribed topics
--  The concept of an MQTT_Item_Id is introduced that maps to a specific value
--  btained from a subscribed topic and the formatting operations that need to
--  be applied. Note: only a single level json file is suppotred, no nesting
--  and no arrays.

--  Author    : David Haley
--  Created   : 24/04/2026
--  Last Edit : 02/06/2026

--  20260601: MQTT_Subscription and Topic_Manager merged.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Topic_Manager is
   
   Topic_Management_File : constant String := "Topic_Management.json";

   subtype Brokers is Unbounded_String;
   subtype Users is Unbounded_String;
   subtype Passwords is Unbounded_String;
   subtype Topics is Unbounded_String;
   subtype Fields is Unbounded_String;

   subtype MQTT_Item_Ids is String;

   type MQTT_Item_Types is (MQTT_String, MQTT_Number, MQTT_U16, MQTT_U32,
     MQTT_Boolean);
   subtype Scaled_Numbers is MQTT_Item_Types range MQTT_U16 .. MQTT_U32;

   subtype Scaling_Factors is Positive;

   subtype Decimal_Places is Natural range 0 .. 3;

   Subscription_Error, Topic_Error : exception;

   -- Subscription Management procedures and functions

   procedure Create_Subscription (Topic : in Topics;
                                  Broker : in Brokers;
                                  User : in Users;
                                  Password : in Passwords);

   --  Creates a new entry or replaces an existing entry.

   procedure Modify_Subscription (Topic : in Topics;
                                  Broker : in Brokers;
                                  User : in Users;
                                  Password : in Passwords);

   --  Topic must already exist, allows changes to Broker, User and Password.
   --  Existing value is retained if an empty string is entered, otherwise the
   --  new value is saved.

   procedure Delete_Subscription (Topic : in Topics);

   --  Deletes the specified topic.

   function Get_Broker (Topic : in Topics) return Brokers;
   --  Returns the broker's host name, from which to subscribe.

   function Get_User (Topic : in Topics) return Users;
   --  Returns the user name to for login.

   function Get_Password (Topic : in Topics) return Passwords;
   -- Returns the password associated with the user.

   --  Note all Get functions raise a Subscription_Error exception if a
   --  there is not subscription recorded for that topic.

   function Topic_Exists (Topic : in Topics) return Boolean;
   --  Returns true if a subscription exists for the topic.

   procedure Put_Subscriptions;

   --  Lists subscriptions to standard output.

   --  The create procedures below will overite stored information for the
   --  relevant item if it already exists  

   procedure Create_String_Item (MQTT_Item_Id : in MQTT_Item_Ids;
                                 Topic : in Topics;
                                 Field : Fields);

   --  Creates a new String_Item defining the topic and the field from which
   --  the string is to be retrieved.  

   procedure Create_Number_Item (MQTT_Item_Id : in MQTT_Item_Ids;
                                 Topic : in Topics;
                                 Field : in Fields);

   --  Creates a new Number_Item defining the topic and the field from which
   --  the number is to be retrieved. The assumption is that the number is
   --  directly capable of being displayed.

   procedure Create_Scaled_Number_Item (MQTT_Item_Id : in MQTT_Item_Ids;
                                        Topic : in Topics;
                                        Field : in Fields;
                                        Scaled_Number : in Scaled_Numbers;
                                        Scaling_Factor : in Scaling_Factors
                                          := 1;
                                        Decimal_Place : in Decimal_Places := 0;
                                        Is_Signed : in Boolean := False);

   --  Creates a new Scaled_Number_Item defining the topic and the field from
   --  which the number is to be retrieved. This is intended to be used to
   --  take a value that may have come more or less directly from a source
   --  such as a modbus register and display it scaled to real world units.
   --  the source value read in can be treated as a signed value (twos
   --  complement), converted to an integer and by scaling and  controling
   --  the number of decimal places converted to a fixed point representation.

   procedure Create_Boolean_Item (MQTT_Item_Id : in MQTT_Item_Ids;
                                  Topic : in Topics;
                                  Field : in Fields;
                                  True_Text : in Unbounded_String :=
                                   To_Unbounded_String ("true");
                                  False_Text : in Unbounded_String :=
                                   To_Unbounded_String ("false"));

   --  Creates a new Boolean_Item defining the topic and the field from which
   --  the Boolean is to be retrieved. True_Text and False_Text define the
   --  text to be displayed wnen the boolean is true and false respectively.
   --  For example when True display on and when false display off etc.

   procedure Delete_Item (MQTT_Item_Id : in MQTT_Item_Ids);

   -- Deletes an item of any type.

   function Get_For_Display (MQTT_Item_Id : in MQTT_Item_Ids) return String;

   --  Gets the text to be displayed based on the most recent value obtained
   --  from the subscribed topic

   procedure List_Items;

   --  Lists all item Id, with the topic and field, one per line.

   procedure Put_Item (MQTT_Item_Id : in MQTT_Item_Ids);

   --  Uses the 'Image attribute to display an Item Id's data

   function Item_Id_Exists (MQTT_Item_Id : in MQTT_Item_Ids) return Boolean;

   --  Returns True if Item_Id has been defined.

   function Item_Type (MQTT_Item_Id : in MQTT_Item_Ids) return MQTT_Item_Types;

   --  Returns the type of item represented by Item_Id

   function File_Exists return Boolean;

   --  Returns true if the topic file exists and is an ordinary file.

   procedure Read_Topics;

   --  Reads in the an existing configuration file.

   procedure Write_Topics;

   --  Writes a new or over writes an existing configuration file.
   
end Topic_Manager;