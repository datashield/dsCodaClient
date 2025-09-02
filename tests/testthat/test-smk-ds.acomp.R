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

#     ds.Surv(time = "D$starttime", time2 = "D$endtime", event = "D$cens", objectname = "surv_object", type = "counting")
#     ds.survfit(formula = "surv_object~1", objectname = "fit_surv_object")
    ds.rcs(x = "D$age.60", knots = 5, objectname = "age_rcs")

    # TODO: Fix error
    expect_error(ds.Predict(fit="age_rcs", age=30:70, sex="both", conf.int=0.95, ref.zero=TRUE, objectname="pred_obj"))
    print(datashield.errors())

    # TODO: Fix error
    res <- expect_error(dsSurvivalClient::ds.acomp(pred_obj = "pred_obj", line_color = "darkblue", x_label = "BMI", event_n = 1000))
    print(datashield.errors())

#    expect_null(res)
})

#
# Done
#

context("ds.acomp::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "age_rcs"))
})

# disconnect.studies.dataset.d()
disconnect.studies.dataset.survival()

context("ds.acomp::smk::done")
