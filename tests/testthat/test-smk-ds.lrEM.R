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

context("ds.lrEM::smk::setup")

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

context("ds.lrEM::smk simple example")
test_that("simple example",  { 

    expect_error(ds.matrix(mdata = "D$servtime", from = "serverside.vector", nrows.scalar = 5, ncols.scalar = 9, newobj = "D_mat"))
    print(datashield.errors())

    expect_error(ds.lrEM(X = "D_mat", objectname = "D_lr"))
    print(datashield.errors())

#    res.class <- ds.class("D_lr")

#    expect_length(res.class, 3)
#    expect_true(all("acomp" %in% res.class[1]))
#    expect_true(all("acomp" %in% res.class[2]))
#    expect_true(all("acomp" %in% res.class[3]))
})

#
# Done
#

context("ds.lrEM::smk::shutdown")

test_that("shutdown", {
#    ds_expect_variables(c("D", "D_mat", "D_lr"))
    ds_expect_variables(c("D"))
})

# disconnect.studies.dataset.d()
disconnect.studies.dataset.survival()

context("ds.lrEM::smk::done")
