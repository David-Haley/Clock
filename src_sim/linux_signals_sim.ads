-- Simulation stub spec for Linux_Signals.
-- Removes pragma Interrupt_Handler to avoid GNAT deadlocking during
-- elaboration of protected objects with interrupt-task-level priority
-- (lock ordering cycle between the Ada interrupt manager and the protected
-- object mutex on this GNAT/Linux version).

package Linux_Signals is

   protected Handlers is
      procedure Install;
      function Signal_Stop return Boolean;
      procedure Remove;
   private
      SIGTERM_Received : Boolean := False;
   end Handlers;

   function Ctrl_C_Stop return Boolean;

end Linux_Signals;
