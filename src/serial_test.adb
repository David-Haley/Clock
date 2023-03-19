with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Real_Time; use Ada.Real_Time;
with GNAT.Serial_Communications; use GNAT.Serial_Communications;
with Interfaces; use Interfaces;

procedure Serial_Test is

   Message_Start : constant Stream_Element := 2#10101010#;
   Message_End : constant Stream_Element := 2#01010101#;
   USB_Port : Port_Name := "/dev/ttyUSB0";

   subtype Grey_Scales is Unsigned_16 range 0 .. 4095;
   subtype LED_Indices is Unsigned_16 range 0 .. 15;

   Port : Serial_Port;

   task Read_Serial is
      entry Start_Reading;
      entry Stop_Reading;
   end  Read_Serial;

   task body Read_Serial is

      Read_Interval : constant Time_Span := Microseconds (1000);
      Exit_Now : Boolean := False;
      Next_Time : Time;
      RX_Buffer : Stream_Element_Array (1 .. 16);
      Last : Stream_Element_Offset;

   begin -- Read_Serial
      accept Start_Reading do
         Next_Time := Clock + Read_Interval;
      end Start_Reading;
      while not Exit_Now loop
         select
            accept Stop_Reading do
               Exit_Now := True;
            end Stop_Reading;
         or
            delay until Next_Time;
            loop
               Read (Port, RX_Buffer, Last);
               for I in Stream_Element_Offset range RX_Buffer'First .. Last loop
                  Put (Character'Val (RX_Buffer (I)));
               end loop; -- I in Stream_Element_Offset range RX_Buffer'First .. Last
               exit when Last < RX_Buffer'First;
            end loop; -- read fron serial port
            Next_Time := Clock + Read_Interval;
         end select;
      end loop;  -- not Exit_Now
      Put_Line ("Terminating reading task");
   end Read_Serial;

   Grey_Scale : Grey_Scales;
   TX_Buffer : Ada.Streams.Stream_Element_Array (0 .. 1);

begin -- Serial_Test
   Put_Line ("Opening " & String (USB_Port));
   Open (Port, USB_Port);
   Put_Line ("Setting baud rate");
   Set (Port, B115200); -- every thing else defaults
   Put_Line ("Start reading task");
   Read_Serial.Start_Reading;
   for I in Grey_Scales range 0 .. 255 loop
      TX_Buffer (0) := Message_Start;
      TX_Buffer (1) := Message_Start;
      Write (Port, TX_Buffer);
      for J in LED_Indices loop
         Grey_Scale := I * (LED_Indices'Last + 1) + J;
         TX_Buffer (0) := Stream_Element (Grey_Scale / 256);
         TX_Buffer (1) := Stream_Element (Grey_Scale mod 256);
         Write (Port, TX_Buffer);
      end loop; -- J in LED_Indices
      TX_Buffer (0) := Message_End;
      TX_Buffer (1) := Message_End;
      Write (Port, TX_Buffer);
      delay 0.25;
   end loop; --I in Grey_Scales range 0 .. 255
   Put_Line ("Stopping reading task");
   Read_Serial.Stop_Reading;
   Put_Line ("Ending test");
end Serial_Test;
