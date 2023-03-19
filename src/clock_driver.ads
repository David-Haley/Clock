-- Instantiation of Clock_LEDs
-- previously in LED_Declarations removes to allow LED_Declarations to be used
-- by the user interface;
-- Author    : David Haley
-- Created   : 16/07/2019
-- Last Edit : 09/06/2022

-- 20220609 : Port to 64 bit native compiler, Driver_Types renamed to
-- TLC5940_Driver_Types.

with RPi_GPIO; use RPi_GPIO;
with TLC5940_Driver_Types; use TLC5940_Driver_Types;
with LED_Declarations; use LED_Declarations;
with TLC5940;

package Clock_Driver is

   package Clock_LEDs is new TLC5940 (Chips => LED_Drivers,
                                      Display_7 => Display_Digits,
                                      VPRG_Pin => Gen0,
                                      XLAT_Pin => Gen1,
                                      Blank_Pin => Gen2);

   X_Error : constant GPIO_Pins := Gen3; -- Error flag input to RPI

end Clock_Driver;
