--  This package manages the linkage between items displayable by the IOT clock
--  and subscribed topics. The concept of an MQTT_Item_Id is introduced that
--  maps to a specific value obtained from a subscribed topic and the
--  formatting operations that need to be applied. Note: only a single level
--  json file is suppotred, no nesting and no arrays.

--  Author    : David Haley
--  Created   : 24/04/2026
--  Last Edit : 14/04/2026

with MQTT_Subscription; -- Required for Topic declaration.
with Interfaces; use Interfaces;

package Topic_Manager is
   
   Topic_Management_File : constant String := "Topic_Management.json";
   
   subtype MQTT_Item_ids is String;

   type MQTT_Item_Types is (MQTT_String, MQTT_Number, MQTT_U16, MQTT_U32, MQTT_Boolean);

   subtype Scaling_Factors is Positive;

   subtype Decimal_Places is Natural range 0 .. 3;  


private
   
end Topic_Manager;