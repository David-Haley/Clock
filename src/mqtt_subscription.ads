--  This package manages the topics that the IOT_Clock is subscribed to.
--  For each topic the associated broker (host name), user and password is
--  is stored and retrieved from a configuration file. The passwords are not
--  stored as plain text but have some simple encription applied.

--  Author    : David Haley
--  Created   : 17/04/2026
--  Last Edit : 18/04/2026

package MQTT_Subscription is

   MQTT_Subscription_File : constant String := "MQTT_Subscription.csv";

   subtype Topics is String;
   subtype Brokers is String;
   subtype Users is String;
   subtype Passwords is String;

   procedure Create (Topic : in Topics;
                     Broker : in Brokers;
                     User : in Users;
                     Password : in Passwords);

   --  Creates a new entry or replaces an existing entry.

   procedure Modify (Topic : in Topics;
                     Broker : in Brokers;
                     User : in Users;
                     Password : in Passwords);

   --  Topic must already exist, allows changes to Broker, User and Password.
   --  Existing value is retained if an empty string is entered, otherwise the
   --  new value is saved.

   procedure Delete (Topic : in Topics);

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

   function File_Exists return Boolean;
   --  Returns true if the subscription configuration exixts;

   procedure Read_Subscription;
   --  Reads an existing subscription file, raise a Subscription_Error
   --  exception if the file cannot be read or errors occur whilst reading
   --  the file.

   procedure Write_Subscription;
   --  Writes (or overwrites) a subscription file, raise a Subscription_Error
   --  exception if the file is not written successfully.

   procedure Put_Subscriptions;

   --  Lists subscriptions to standard output.

   Subscription_Error : exception;

end MQTT_Subscription;