{
{-# LANGUAGE FieldSelectors, NoOverloadedRecordUpdate, NoRebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-do-bind -Wno-name-shadowing #-}

module Lexer (
    Alex (..),
    AlexInput,
    AlexPosn (..),
    AlexState (..),
    alexMonadScan,
    runAlex) where

-- base
import Control.Monad           (Monad (..))
import Data.Bool               (Bool (..), (&&), (||), otherwise)
import Data.Either             (Either (..))
import Data.Function           (($), const)
import Data.Functor            (Functor (..))
import Data.Eq                 (Eq (..))
import Data.List               ((++))
import Data.Maybe              (Maybe (..))
import Data.Ord                (Ord (..))
import Data.String             (String ())
import Text.Read               (Read (..))
import Text.Show               (Show (..))

-- local
import GHC.Exts.Rebindable
import {-# SOURCE #-} Handlers

import Aux1 (
    AlexUserState (..),
    Token (..),
    alexInitUserState)

}

%wrapper "monadUserState-bytestring"

$ab_left    = \<
$ab_right   = \>
$amp        = \&
$at         = \@
$backtick   = \`
$bang       = \!
$b_slash    = \\
$cb_left    = \{
$cb_right   = \}
$colon      = \:
$comma      = \,
$cr         = \x0D
$crlf       = \x0D0A
$dash       = \-
$div        = \/
$dollar     = \$
$dq         = \"
$equal      = \=
$f_slash    = \/
$gt         = \>
$hash       = \#
$lf         = \x0A
$lt         = \<
$minus      = \-
$mult       = \*
$p_left     = \(
$p_right    = \)
$percent    = \%
$period     = \.
$pipe       = \|
$plus       = \+
$pound      = \#
$question   = \?
$sb_left    = \[
$sb_right   = \]
$semicolon  = \;
$space      = \x20
$sq         = \'
$star       = \*
$tab        = \x09
$tilde      = \~
$underscore = \_

$c_printable =                                                      --   [1]
    [                                                               --
        $space $lf $cr \x20-\x7E        -- 8 bit                    --
        \x85 \xA0-\xD7FF \xE000-\xFFFD  -- 16 bit                   --
        \x10000-\x10FFFF                -- 32 bit                   --
    ]                                                               --
$nb_json = [\x9 \x20-\x10FFFF]                                      --   [2]
$c_byte_order_mark = [\x0 \xFE \xFF \xEF \xBB \xBF]                 --   ***   [3]
@c_byte_order_mark =                                                --   [3]
    \x00 \x00 \xFE \xFF	      | -- UTF-32BE (Explicit BOM)          --
    \x00 \x00 \x00 [\x0-\xFF] | -- UTF-32BE (ASCII first character) --
    \xFF \xFE \x00 \x00	      | -- UTF-32LE (Explicit BOM)          --
    [\x0-\xFF] \x00 \x00 \x00 | -- UTF-32LE (ASCII first character) --
    \xFE \xFF	 	 	      | -- UTF-16BE (Explicit BOM)          --
    \x00 [\x0-\xFF]           | -- UTF-16BE (ASCII first character) --
    \xFF \xFE                 | -- UTF-16LE (Explicit BOM)          --
    [\x0-\xFF] #x00           | -- UTF-16LE (ASCII first character) --
    \xEF \xBB \xBF              -- UTF-8                            --
$c_sequence_entry = $dash                                           --   [4]
$c_mapping_key = $question                                          --   [5]
$c_mapping_value = $colon                                           --   [6]
$c_collect_entry = $comma                                           --   [7]
$c_sequence_start = $sb_left                                        --   [8]
$c_sequence_end = $sb_right                                         --   [9]
$c_mapping_start = $cb_left                                         --  [10]
$c_mapping_end = $cb_right                                          --  [11]
$c_comment = $hash                                                  --  [12]
$c_anchor = $amp                                                    --  [13]
$c_alias = $star                                                    --  [14]
$c_tag = $bang                                                      --  [15]
$c_literal = $pipe                                                  --  [16]
$c_folded = $gt                                                     --  [17]
$c_single_quote = $sq                                               --  [18]
$c_double_quote	= $dq                                               --  [19]
$c_directive = $percent                                             --  [20]
$c_reserved = [$at $backtick]                                       --  [21]
$c_indicator =                                                      --  [22]
    [                                                               --
        $c_sequence_entry $c_mapping_key $c_mapping_value           --
        $c_collect_entry $c_sequence_start $c_sequence_end          --
        $c_mapping_start $c_mapping_end $c_comment                  --
        $c_anchor $c_alias $c_tag $c_literal $c_folded              --
        $c_single_quote $c_double_quote $c_directive                --
        $c_reserved                                                 --
    ]                                                               --
$c_flow_indicator =                                                 --  [23]
    [                                                               --
        $comma $sb_left $sb_right $cb_left $cb_right                --
    ]                                                               --
$b_line_feed = $lf	                                                --  [24]
$b_carriage_return = $cr	                                        --  [25]
$b_char = [$b_line_feed $b_carriage_return]                         --  [26]
$nb_char = $c_printable # [$b_char $c_byte_order_mark]              --  [27]
@b_break =                                                          --  [28]
    $b_carriage_return $b_line_feed | -- DOS, Windows               --
    $b_carriage_return              | -- MacOS upto 9.x             --
    $b_line_feed                      -- UNIX, MacOS X              --
@b_as_line_feed = @b_break                                          --  [29]
@b_non_content = @b_break                                           --  [30]	
$s_space = $space	                                                --  [31]
$s_tab = $tab 	                                                    --  [32]
$s_white = [$s_space $s_tab]                                        --  [33]
$ns_char = $nb_char # $s_white                                      --  [34]
$ns_dec_digit = [0-9]                                               --  [35]
$ns_hex_digit = [$ns_dec_digit A-F a-f]                             --  [36]
$ns_ascii_letter = [A-Z a-z]                                        --  [37]
$ns_word_char =	[$ns_dec_digit $ns_ascii_letter $dash]              --  [38]
$ns_uri_char =                                                      --  ****  [39]
    [                                                               --  ****  [39]
        $ns_word_char $hash $semicolon $f_slash $question           --  ****  [39]
        $colon $at $amp $equal $plus                                --  ****  [39]
        $dollar $comma $underscore $period $bang                    --  ****  [39]
        $tilde $star $sq $p_left $p_right $sb_left $sb_right        --  ****  [39]
    ]                                                               --  ****  [39]
@ns_uri_escaped = $percent $ns_hex_digit $ns_hex_digit              --  ****  [39]
@ns_uri_char = @ns_uri_escaped | $ns_uri_char                       --  [39]
@ns_tag_char =                                                      --  [40]
    @ns_uri_escaped |                                               --
    $ns_uri_char # [$bang $c_flow_indicator]                        --
$c_escape = $b_slash                                                --  [41]
$ns_esc_null = 0                                                    --  [42]
    -- Escaped ASCII null (#x0) character.                          --
$ns_esc_bell = a                                                    --  [43]
    -- Escaped ASCII bell (#x7) character.                          --
$ns_esc_backspace = b                                               --  [44]
    -- Escaped ASCII backspace (#x8) character.                     --
$ns_esc_horizontal_tab = [t $tab]                                   --  [45]
    -- Escaped ASCII horizontal tab (#x9) character.                --
    -- This is useful at the start or the end of a line             --
    -- to force a leading or trailing tab to become                 --
    -- part of the content.                                         --
$ns_esc_line_feed = n                                               --  [46]
    -- Escaped ASCII line feed (#xA) character.                     --
$ns_esc_vertical_tab = v                                            --  [47]
    -- Escaped ASCII vertical tab (#xB) character.                  --
$ns_esc_form_feed = f                                               --  [48]
    -- Escaped ASCII form feed (#xC) character.                     --
$ns_esc_carriage_return = r                                         --  [49]
    -- Escaped ASCII carriage return (#xD) character.               --
$ns_esc_escape = e                                                  --  [50]
    -- Escaped ASCII escape (#x1B) character.                       --
$ns_esc_space = $space                                              --  [51]
    -- Escaped ASCII space (#x20) character. This is                --
    -- useful at the start or the end of a line to                  --
    -- force a leading or trailing space to become part             --
    -- of the content.                                              --
$ns_esc_double_quote = $dq                                          --  [52]
    -- Escaped ASCII double quote (#x22).                           --
$ns_esc_slash = $f_slash                                            --  [53]
    -- Escaped ASCII slash (#x2F), for JSON compatibility.          --
$ns_esc_backslash = $b_slash                                        --  [54]
    -- Escaped ASCII back slash (#x5C).                             --
$ns_esc_next_line =	N                                               --  [55]
    -- Escaped Unicode next line (#x85) character.                  --
$ns_esc_non_breaking_space = $underscore                            --  [56]
    -- Escaped Unicode non_breaking space (#xA0) character.         --
$ns_esc_line_separator = L                                          --  [57]
    -- Escaped Unicode line separator (#x2028) character.           --
$ns_esc_paragraph_separator = P                                     --  [58]
    -- Escaped Unicode paragraph separator (#x2029) character.      --
@ns_esc_8_bit = x $ns_hex_digit {2}                                 --  [59]
    -- Escaped 8-bit Unicode character.                             --
@ns_esc_16_bit = u $ns_hex_digit {4}                                --  [60]
    -- Escaped 16-bit Unicode character.                            --
@ns_esc_32_bit = U $ns_hex_digit {8}                                --  [61]
    -- Escaped 32-bit Unicode character.                            --
@c_ns_esc_char =                                                    --  [62]
    $c_escape (                                                     --
        $ns_esc_null |                                              --
        $ns_esc_bell |                                              --
        $ns_esc_backspace |                                         --
        $ns_esc_horizontal_tab |                                    --
        $ns_esc_line_feed |                                         --
        $ns_esc_vertical_tab |                                      --
        $ns_esc_form_feed |                                         --
        $ns_esc_carriage_return |                                   --
        $ns_esc_escape |                                            --
        $ns_esc_space |                                             --
        $ns_esc_double_quote |                                      --
        $ns_esc_slash |                                             --
        $ns_esc_backslash |                                         --
        $ns_esc_next_line |                                         --
        $ns_esc_non_breaking_space |                                --
        $ns_esc_line_separator |                                    --
        $ns_esc_paragraph_separator |                               --
        @ns_esc_8_bit |                                             --
        @ns_esc_16_bit |                                            --
        @ns_esc_32_bit                                              --
    )                                                               --
-- s_indent(n)                            -- * implement in parser  --  [63]
-- s_indent(m<n)                          -- * implement in parser  --  [64]
-- s_indent(m≤n)                          -- * implement in parser  --  [65]
-- s_separate_in_line                     -- * implement in parser  --  [66]
-- s_line_prefix                          -- * implement in parser  --  [67]
-- s_block_line_prefix                    -- * implement in parser  --  [68]
-- s_flow_line_prefix                     -- * implement in parser  --  [69]
-- l_empty                                -- * implement in parser  --  [70]
-- b_l_trimmed                            -- * implement in parser  --  [71]
@c_nb_comment_text = “#” $nb_char*	                                --  [75]
@b_comment = @b_non_content                                         --  [76]
-- s_b_comment =                          -- * implement in parser  --  [77]

$ns_anchor_char = $ns_char # $c_flow_indicator                      -- [102]
@ns_anchor_name	= $ns_anchor_char+                                  -- [103]
@c_ns_anchor_property = $amp @ns_anchor_name                        -- [101]
@c_ns_alias_node = $star @ns_anchor_name                            -- [104]

$c_scalar_special_first = $c_indicator
$c_scalar_regular_first = $ns_char # $c_scalar_special_first
@ns_scalar_char =
    $ns_char # $colon |
    $colon $ns_char
@nb_scalar_char =
    $nb_char # $colon |
    $colon $ns_char
@scalar = 
    $c_scalar_regular_first @nb_scalar_char* |
    $c_scalar_special_first @ns_scalar_char @nb_scalar_char*
@impinged_scalar = @ns_scalar_char @nb_scalar_char*

-- fls: Flow Style
-- bks: Block Style
-- ots: Other Style
-- sco: Scalar Options
:-
      ^ $s_space+                                    { recordSpaces }
      @b_break                                       { 
            do
                r <- grab BreakNonContent
                resetSpaces
                return r
        }
      $c_alias                                       { grab Alias }
      $c_alias ^ @ns_anchor_name                     { grab AnchorName }
      $c_anchor                                      { grab Anchor }
      $c_anchor ^ @ns_anchor_name                    { grab AnchorName }
      @c_nb_comment_text                             { grab Comment }
      $s_white+                                      { grab SeparateInline }
      $c_sequence_start                              {
        \i l -> do
            pushStartcode fls
            grab SequenceStart i l }
      $c_mapping_start                               {
        \i l -> do
            pushStartcode fls
            grab MappingStart i l }
<0>   ()                                             { begin bks }
<bks> $c_sequence_entry                              { grab SequenceEntry }
<bks> $c_mapping_key                                 { grab MappingKey }
<bks> $c_mapping_value                               { grab MappingValue }
<bks> @impinged_scalar / $c_mapping_value            { grab String }
<bks> @scalar                                        { grab String }
<fls> $c_collect_entry                               { grab CollectEntry }
<fls> $c_sequence_end                                {
        \i l -> do
            popStartcode
            grab SequenceEnd i l }
<fls> $c_mapping_end                                 {
        \i l -> do
            popStartcode
            grab MappingEnd i l }
