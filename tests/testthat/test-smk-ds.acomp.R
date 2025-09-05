#-------------------------------------------------------------------------------
# Copyright (c) 2025 ProPASS Consortium. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

context("ds.acomp::smk::setup")

# load "d" test data set
# connect.studies.dataset.d(list('ID', 'age', 'sex', 'smoke', 'fruit', 'veg', 'edu', 'eth', 'job', 'slf_hlth', 'alc', 'mobility', 'fasting', 'med_lipid', 'med_bp', 'med_glucose', 'prev_cvd', 'prev_ht', 'prev_bronchitis', 'body_fat_percent'))
# load "survival" test data set
connect.studies.dataset.survival(list('id', 'study.id', 'time.id', 'starttime', 'endtime', 'survtime', 'cens', 'age.60', 'female', 'noise.56', 'pm10.16', 'bmi.26'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.acomp::smk")
test_that("simple example",  {

    ds.acomp(X = "D$age.60", objectname = "age_comp")

    res.class <- ds.class("age_comp")

    expect_length(res.class, 3)
    expect_true(all("acomp" %in% res.class[1]))
    expect_true(all("acomp" %in% res.class[2]))
    expect_true(all("acomp" %in% res.class[3]))
})

#
# Done
#

context("ds.acomp::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "age_comp"))
})

# disconnect.studies.dataset.d()
disconnect.studies.dataset.survival()

context("ds.acomp::smk::done")
