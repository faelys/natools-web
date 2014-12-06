with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Pages.Commands is

   P : constant array (0 .. 2) of Natural :=
     (1, 4, 8);

   T1 : constant array (0 .. 2) of Unsigned_8 :=
     (7, 9, 11);

   T2 : constant array (0 .. 2) of Unsigned_8 :=
     (6, 2, 16);

   G : constant array (0 .. 16) of Unsigned_8 :=
     (6, 0, 0, 7, 0, 0, 0, 0, 0, 0, 5, 6, 0, 0, 2, 3, 5);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 17;
         F2 := (F2 + Natural (T2 (K)) * J) mod 17;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 8;
   end Hash;

end Natools.Static_Maps.Web.Pages.Commands;
