from shiny import App, render, ui

app_ui = ui.page_fluid(
    ui.h3("Best-Case Doula Care Scenario"),
    ui.input_numeric("preterm_births", "Number Babies Preterm", 349),
    ui.input_numeric("total_births", "Number of Births", 1649),
    ui.input_numeric("preterm_reduction_rate", "Preterm Reduction Rate (%)", 1.6, step=0.1),
    ui.input_slider("normal_birth_cost", "Cost of Birth (Normal, No Complications)", 10000, 50000, 10000),
    ui.output_text_verbatim("preterm_universalavoided"),
    ui.output_text_verbatim("preterm_universalnormalbirthcost"), 
    ui.output_text_verbatim("preterm_universaldoulabirthcost"), 
    ui.output_text_verbatim("preterm_universalavoidedcost")
)

def server(input, output, session):
    @output
    @render.text
    def preterm_universalavoided():
        new_r = (input.preterm_births()/input.total_births())- (input.preterm_reduction_rate() / 100)
        new_p = round(input.total_births()*new_r, 0)
        return(f'{new_p} Births Before Full Term and {round(input.preterm_births()-new_p, 0)} Babies Prevented From Preterm with Doulas')

    @output
    @render.text
    def preterm_universalnormalbirthcost():
        c_norm = input.normal_birth_cost()*(input.total_births() - input.preterm_births())
        c_p = (input.normal_birth_cost()*4)*(input.preterm_births())
        c_total = round(c_norm + c_p, 0) 
        return(f'${c_total:,} Total Medical Costs of All Non-Doula Births')

    @output
    @render.text
    def preterm_universaldoulabirthcost():
        c_norm_doula = input.normal_birth_cost()*(input.total_births()-(((input.preterm_births()/input.total_births())-(input.preterm_reduction_rate()/100))*input.total_births()))
        c_p_doula = (input.normal_birth_cost()*4)*(((input.preterm_births()/input.total_births())-(input.preterm_reduction_rate()/100))*input.total_births())
        c_total_doula = round(c_norm_doula + c_p_doula, 0)
        return(f'${c_total_doula:,} Total Medical Costs of All Doula Births')
    
    @output
    @render.text
    def preterm_universalavoidedcost():
        # Normal Costs
        c_norm = input.normal_birth_cost()*(input.total_births()-input.preterm_births())
        c_p = (input.normal_birth_cost()*4)*(input.preterm_births())
        c_total = c_norm + c_p
        # Doula Supported Costs
        c_norm_doula = input.normal_birth_cost()*(input.total_births()-(((input.preterm_births()/input.total_births())-(input.preterm_reduction_rate()/100))*input.total_births()))
        c_p_doula = (input.normal_birth_cost()*4)*(((input.preterm_births()/input.total_births())-(input.preterm_reduction_rate()/100))*input.total_births())
        c_total_doula = c_norm_doula + c_p_doula
        # Difference Normal and Doula Costs
        c_total_difference = round(c_total - c_total_doula, 0)
        return(f'${c_total_difference:,} Savings from Doula Reduction in Preterm Births')

app = App(app_ui, server)
