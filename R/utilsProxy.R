# DsFeatFreqComp – Dataset Feature Frequency Comparison (Proxy Utulity Scripts)
# Copyright (C) 2017-2018 Gürol Canbek
# This file is licensed under
#
#   GNU Affero General Public License v3.0, GNU AGPLv3
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
# See the license file in <https://github.com/gurol/dsfeatfreqcompt>
#
#' @author Gürol Canbek, <gurol44@gmail.com>
#' @references G. Canbek et al. (2018) Gaining New Insight in Datasets via Multiple Binary-Feature Frequency Ranks with A Benign/Malign Mobile Apps Example, Journal of Machine Learning Research (JMLR) (Submitted)
#' (\href{http://gurol.canbek.com/Publications/}{Publications by Gurol Canbek})
#' @title Proxy Utulity Scripts

#' Stop script run and show a (custom) message to user to press ENTER
#'
#' Show a given message with 'Press [enter] to continue' statement and wait for
#' the user interaction. It is useful for pausing script run
#'
#' @param message custom message text to display
pressEnterToContinue<-function(message='')
{
  invisible(readline(prompt=paste0(message, 'Press [enter] to continue')))
}
