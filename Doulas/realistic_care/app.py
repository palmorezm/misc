from shiny import App, render, ui

app_ui = ui.page_fluid(
    ui.h3("Realistic Doula Care Scenario"),
    # Results
    ui.row(
        # Inputs
        ui.column(6,
            ui.input_slider("n_doulas", "Doulas Available", 0, 100, 6), 
            ui.input_slider("bpd", "Births Per Doula (Monthly)", 0, 5, 1), 
            ui.input_slider("pbr", "Preterm Birth Rate (%)", 0, 50, 21.16, step=0.1),
            ui.input_slider("r", "Preterm Reduction Rate (%)", 0, 10, 1.6, step=0.1),
            ui.input_slider("mgp", "Preterm Average Gest.", 20, 37, 35.52, step=0.1)), 
        ui.column(6, 
            ui.input_numeric("full_term", "Full Term (Weeks)", 37),
            ui.input_numeric("cost_per_day", "Daily Preterm Cost", 3000),
            ui.output_text_verbatim("possible_births"),
            ui.output_text_verbatim("preterm_realbirths"), 
            ui.output_text_verbatim("preterm_realavoided"), 
            ui.output_text_verbatim("preterm_gest"),
            ui.output_text_verbatim("preterm_realavoidedcost"))
    )
    
)


def server(input, output, session):

    # Total Number of Births Possible in Year
        # With Availability of Doulas and Limited Births per Month
    @output
    @render.text
    def possible_births():
        return(f"{input.n_doulas()*input.bpd()*12} Doula Supported Births") # births with doula care

    # Total Preterm Births 
        # From Doula-Supported Population
    @output
    @render.text
    def preterm_realbirths():
        return(f"{round(input.n_doulas()*input.bpd()*12*(input.pbr()/100), 0)} Preterm Births with Doulas") # births before 37 weeks
    
    # Total Expenses Avoided 
        # From Reduction in Preterm Births
    @output
    @render.text
    def preterm_realavoided():
        bpy = (input.n_doulas()*input.bpd()*12)
        total_preterm_normal = (bpy*input.pbr()) 
        total_preterm_doula = bpy*(input.pbr()-(input.r()/100))
        avoided_preterm = total_preterm_normal - total_preterm_doula
        return(f"{round(avoided_preterm, 2)} Births Prevented from Preterm")

    # Mean Gestation Period
    @output
    @render.text
    def preterm_gest():
        days = (input.full_term()*7)-(input.mgp()*7) # 37 weeks = full gest_
        return(f"{days} Days on Average Born Before Full Term") # answer in days

    # Avoided Costs Considering Doula Availability
    @output
    @render.text
    def preterm_realavoidedcost():
        bpy = (input.n_doulas()*input.bpd()*12)
        avoided = (bpy*input.pbr()) - (bpy*(input.pbr()-(input.r()/100)))
        average_days_preterm = (input.full_term()*7)-(input.mgp()*7)
        avoidedcost = round(avoided*input.cost_per_day()*average_days_preterm, 0)
        return(f"${avoidedcost:,} Savings from Reduction in Preterm Births")

app = App(app_ui, server)