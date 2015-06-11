with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Simple_Pages.Commands is

   P : constant array (0 .. 2) of Natural :=
     (1, 4, 8);

   T1 : constant array (0 .. 2) of Unsigned_8 :=
     (7, 17, 14);

   T2 : constant array (0 .. 2) of Unsigned_8 :=
     (1, 2, 13);

   G : constant array (0 .. 27) of Unsigned_8 :=
     (3, 0, 4, 0, 8, 4, 1, 0, 0, 5, 9, 0, 0, 0, 0, 0, 0, 7, 3, 0, 0, 0, 10,
      3, 0, 0, 0, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 28;
         F2 := (F2 + Natural (T2 (K)) * J) mod 28;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 13;
   end Hash;

end Natools.Static_Maps.Web.Simple_Pages.Commands;
