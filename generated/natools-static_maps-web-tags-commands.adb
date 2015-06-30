with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Tags.Commands is

   P : constant array (0 .. 1) of Natural :=
     (1, 11);

   T1 : constant array (0 .. 1) of Unsigned_8 :=
     (18, 14);

   T2 : constant array (0 .. 1) of Unsigned_8 :=
     (7, 27);

   G : constant array (0 .. 36) of Unsigned_8 :=
     (0, 0, 0, 2, 0, 0, 0, 7, 0, 12, 0, 3, 1, 14, 1, 0, 3, 0, 5, 7, 8, 0, 0,
      7, 0, 13, 0, 0, 0, 0, 9, 6, 0, 0, 0, 14, 14);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 37;
         F2 := (F2 + Natural (T2 (K)) * J) mod 37;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 18;
   end Hash;

end Natools.Static_Maps.Web.Tags.Commands;
